{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Stream (
  Stream(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Word (Word8)


-- | Input stream of tokens
class Stream s where

  type StreamItem s :: *

  -- | Pull the next item from the stream
  uncons :: s -> Maybe (StreamItem s, s)


instance Stream [a] where
  type StreamItem [a] = a
  uncons = L.uncons


instance Stream T.Text where
  type StreamItem T.Text = Char
  uncons = T.uncons


instance Stream TL.Text where
  type StreamItem TL.Text = Char
  uncons = TL.uncons


instance Stream B.ByteString where
  type StreamItem B.ByteString = Word8
  uncons = B.uncons


instance Stream BL.ByteString where
  type StreamItem BL.ByteString = Word8
  uncons = BL.uncons


-- | A general tuple can't reasonably be a stream, as there would be no way for
-- it to terminate or even rotate.  But a tuple of streams can operate in
-- a simple sequence
instance (Stream a,
  Stream b, StreamItem a ~ StreamItem b)
  => Stream (a, b) where
  type StreamItem (a, b) = StreamItem a
  uncons (a, b) =
    second (, b) <$> uncons a
    <|> second (a, ) <$> uncons b


instance (Stream a,
  Stream b, StreamItem a ~ StreamItem b,
  Stream c, StreamItem a ~ StreamItem c)
  => Stream (a, b, c) where
  type StreamItem (a, b, c) = StreamItem a
  uncons (a, b, c) =
        second ( , b, c) <$> uncons a
    <|> second (a,  , c) <$> uncons b
    <|> second (a, b,  ) <$> uncons c


instance (Stream a,
  Stream b, StreamItem a ~ StreamItem b,
  Stream c, StreamItem a ~ StreamItem c,
  Stream d, StreamItem a ~ StreamItem d)
  => Stream (a, b, c, d) where
  type StreamItem (a, b, c, d) = StreamItem a
  uncons (a, b, c, d) =
        second ( , b, c, d) <$> uncons a
    <|> second (a,  , c, d) <$> uncons b
    <|> second (a, b,  , d) <$> uncons c
    <|> second (a, b, c,  ) <$> uncons d


-- | A stream chain concatenates a series of streams into a single result.
newtype StreamChain a = StreamChain { streamChunks :: [a] }

instance Stream a => Stream (StreamChain a) where
  type StreamItem (StreamChain a) = StreamItem a
  uncons (StreamChain l) = do
    (top, l1) <- L.uncons l
    (item, (remHead, StreamChain remTail)) <- uncons (top, StreamChain l1)
    return (item, StreamChain (remHead : remTail))


instance Monoid (StreamChain a) where
  mempty = StreamChain []
  mappend (StreamChain a) (StreamChain b) = StreamChain (mappend a b)


----
