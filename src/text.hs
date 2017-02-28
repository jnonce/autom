{-# LANGUAGE TypeFamilies #-}

module Text (
  text,
  anyChar,
  category,

  space, lower, upper, alpha, alphaNum, digit,
  letter, number,

  asciiDigit,
  naturalNumber,
  integer
  ) where

import qualified Data.Char as C
import           Data.Monoid
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as T
import Stream
import Machine

-- | Match and extract a literal string
text :: (Monad m, Stream s, Char ~ StreamItem s) => Text -> MachineT s m Text
text t = (fmap T.pack . tokens) s <?> ('\'' : s ++ "'")
  where
    s = T.unpack t

-- | Select any one character
anyChar :: (Monad m, Stream s, Char ~ StreamItem s) => MachineT s m Char
anyChar = anyToken

-- | Determine whether a character is in a set.  Used with satisfies.
inClass :: (Foldable t, Eq a) => t a -> a -> Bool
inClass characters = getAny . foldMap mapped characters
  where
    mapped ch test = Any (test == ch)


-- | Select one character restricted to the given general category
category :: (Monad m, Stream s, Char ~ StreamItem s) => C.GeneralCategory -> MachineT s m Char
category c = satisfies ((==) c . C.generalCategory) <?> show c


space, lower, upper, alpha, alphaNum, digit, letter, number :: (Monad m, Stream s, Char ~ StreamItem s) => MachineT s m Char

-- | Selects any Unicode space character, and the control characters \t, \n, \r, \f, \v.
space = satisfies C.isSpace <?> "space"

-- | Selects lower-case alphabetic Unicode characters (letters).
lower = satisfies C.isLower <?> "lower"

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the single-character form of Lj.
upper = satisfies C.isUpper <?> "upper"

-- | Selects alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters). This function is equivalent to isLetter.
alpha = satisfies C.isAlpha <?> "alpha"

-- | Selects alphabetic or numeric digit Unicode characters.
-- Note that numeric digits outside the ASCII range are selected by this function but not by isDigit.
-- Such digits may be part of identifiers but are not used by the printer and reader to represent numbers.
alphaNum = satisfies C.isAlphaNum <?> "alphaNum"

-- | Selects ASCII digits, i.e. '0'..'9'.
digit = satisfies C.isDigit <?> "digit"

-- | Selects alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters). This function is equivalent to isAlpha.
letter = satisfies C.isLetter <?> "letter"

-- | Selects Unicode numeric characters, including digits from various scripts, Roman numerals, etc.
number = satisfies C.isNumber <?> "number"

-- | Parse an ascii digit and return the integral meaning of the digit
asciiDigit :: (Monad m, Stream s, Char ~ StreamItem s) => MachineT s m Int
asciiDigit = satisfy $ \item -> lookup item asciiDigits
  where
    asciiDigits = ['0'..'9'] `zip` [0..9]


-- | Parse a non-negative integer represented as a stream of digits.
naturalNumber :: (Monad m, Stream s, Char ~ StreamItem s) => MachineT s m Integer
naturalNumber = foldl mergeDigit 0 <$> some asciiDigit
  where
    mergeDigit soFar digit = (soFar * 10) + toInteger digit


-- | Simple decimal integer, allowing for negative values.
integer :: (Monad m, Stream s, Char ~ StreamItem s) => MachineT s m Integer
integer = do
  isNegation <- optional (token '-')
  num <- naturalNumber
  return $ if isJust isNegation
    then negate num
    else num
