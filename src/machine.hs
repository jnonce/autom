{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Machine (
  MachineT(..),
  Machine,
  MachineFault(..),
  runMachine,
  runMachineT,
  faulted,
  inContext,
  (<?>),
  satisfy,
  satisfies,
  eos,
  anyToken,
  token,
  tokens,
  tokenInRange,

  sep,
  sep1,
  terminated,
  terminated1,

  replicateAtMostN,
  replicateMtoN,

   -- Control.Applicative
  Alternative (..),
  optional,
  replicateM,

  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Control.Monad.Trans
import           Data.Maybe (fromMaybe, isNothing)
import Stream

{- | Parser machine

 - s is the machine's stream type
 - m is the underlying monad
 - a is the resultant type

-}
newtype MachineT s m a = MachineT { innerStateMachine :: StateT s (ExceptT MachineFault m) a }
  deriving (Functor, Applicative)

-- | Describes the context for a fault
newtype MachineFault = MachineFault [String] deriving (Eq, Show)

type Machine s = MachineT s Identity


instance Monoid MachineFault where
  mempty = MachineFault []
  mappend (MachineFault a) (MachineFault b) = MachineFault (a ++ b)


instance (Monad m) => Monad (MachineT s m) where
  (MachineT m) >>= f = MachineT (f <$> m >>= innerStateMachine)
  fail str = MachineT (StateT (const flt))
    where
      flt :: Monad m => ExceptT MachineFault m a
      flt = ExceptT . pure . Left $ MachineFault [str]


instance MonadTrans (MachineT s) where
  lift = MachineT . lift . lift


instance Monad m => Alternative (MachineT s m) where
  empty = MachineT $ throwError mempty
  (MachineT x) <|> (MachineT y) = MachineT (x <|> y)


instance Monad m => MonadPlus (MachineT s m) where


-- | Run the given machine over a given stream
runMachineT :: (Monad m, Stream s) => MachineT s m a -> s -> m (Either MachineFault (a, s))
runMachineT (MachineT s) stream = r
  where
    (ExceptT r) = runStateT s stream


-- | Run the given machine over a given stream
runMachine :: (Stream s) => Machine s a -> s -> Either MachineFault (a, s)
runMachine m = runIdentity . runMachineT m


-- | Produce a machine which reads the next input symbol and tests a condition
satisfy :: (Monad m, Stream s) => (StreamItem s -> Maybe a) -> MachineT s m a
satisfy f = MachineT $ do
  st <- get
  case satisfyTop st of
    Nothing -> mzero
    Just (hd, tl) -> put tl >> return hd
  where
    satisfyTop st = do
      (hd, tl) <- uncons st
      r <- f hd
      return (r, tl)


-- | Produce a machine which reads the next input symbol and tests a condition
satisfies :: (Monad m, Stream s, StreamItem s ~ a) => (a -> Bool) -> MachineT s m a
satisfies f = satisfy $ \item ->
  if f item
    then Just item
    else Nothing


-- | Test for End-of-Stream
eos :: (Monad m, Stream s) => MachineT s m ()
eos = inContext "EOS" $ MachineT $ do
  isEOS <- gets (isNothing . uncons)
  guard isEOS


-- | A machine that will always fault with the given message
faulted :: (Monad m) => String -> MachineT s m ()
faulted = MachineT . throwError . MachineFault . pure


-- | Run a machine in a named context.  If parsing fails the message will be
-- inserted into the context stack.
inContext :: (Monad m) => String -> MachineT s m a -> MachineT s m a
inContext name (MachineT m) = MachineT $ catchError m handler
  where
    handler (MachineFault c) = throwError $ MachineFault (name : c)


-- | Infix variant of `inContext`
(<?>) :: (Monad m) => MachineT s m a -> String -> MachineT s m a
(<?>) = flip inContext
infixl 2 <?>


-- | Ensure that the given token is next in stream
token :: (Monad m, Stream s, a ~ StreamItem s, Eq a) => StreamItem s -> MachineT s m a
token t = satisfies (== t)


-- | Ensure that the next token is within a range
tokenInRange :: (Monad m, Stream s, a ~ StreamItem s, Eq a, Enum a) => a -> a -> MachineT s m a
tokenInRange from to = satisfies (\item -> item `elem` [from..to])


-- | Retrieve the next token
anyToken :: (Monad m, Stream s, a ~ StreamItem s) => MachineT s m a
anyToken = satisfy Just


-- | Ensure that a sequence of tokens come next in stream
tokens = mapM token
tokens :: (Monad m, Stream s, Traversable t, Eq a, a ~ StreamItem s) => t a -> MachineT s m (t a)


-- | Zero or more matches of a machine, with a separator between each instance
sep :: (Monad m, Stream s, a ~ StreamItem s) => MachineT s m b -> MachineT s m a -> MachineT s m [a]
sep between item = do
  top <- optional item
  case top of
    Nothing -> return []
    Just t -> (t :) <$> many (between >> item)


-- | One or more matches of a machine, with a separator between each instance
sep1 :: (Monad m, Stream s, a ~ StreamItem s) => MachineT s m b -> MachineT s m a -> MachineT s m [a]
sep1 between item = do
  top <- item
  bottom <- many (between >> item)
  return (top : bottom)


-- | A sequence of zero or more items, each terminated by a given machine
terminated :: (Monad m, Stream s, a ~ StreamItem s) => MachineT s m b -> MachineT s m a -> MachineT s m [a]
terminated after item = many (item <* after)

-- | A sequence of one or more items, each terminated by a given machine
terminated1 :: (Monad m, Stream s, a ~ StreamItem s) => MachineT s m b -> MachineT s m a -> MachineT s m [a]
terminated1 after item = some (item <* after)


-- | Replicate a machine to parse up to a limited number of matches, stopping
-- at the first `empty`.
replicateAtMostN :: (Alternative m) => Int -> m a -> m [a]
replicateAtMostN n m = replicateAtMostN' n
  where
    replicateAtMostN' 0 = pure []
    replicateAtMostN' n = liftA2 (:) m (replicateAtMostN' (n - 1)) <|> pure []


-- | Replicate a machine to parse between some minimum and maximum number of
-- matches.
replicateMtoN :: (Monad m, Alternative m) => Int -> Int -> m a -> m [a]
replicateMtoN l h m = (++) -- Concat a required and an optional part
  <$> replicateM l m       -- Required l
  <*> replicateAtMostN (h - l) m     -- Optional, taking us up to h


--
