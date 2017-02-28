{-# LANGUAGE TypeFamilies #-}

module Stream (
  Stream(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import           Data.Word (Word8)

-- | Input stream of tokens
class Stream s where
  type StreamItem s :: *
  uncons :: s -> Maybe (StreamItem s, s)

instance Stream [a] where
  type StreamItem [a] = a
  uncons [] = Nothing
  uncons (a:l) = Just (a, l)

instance Stream T.Text where
  type StreamItem T.Text = Char
  uncons = T.uncons

instance Stream B.ByteString where
  type StreamItem B.ByteString = Word8
  uncons = B.uncons
