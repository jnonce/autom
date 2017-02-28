{-# LANGUAGE TypeFamilies #-}

module ByteString (
  word8,
  word16LE,
  word16BE,
  word32LE,
  word32BE,
  word64LE,
  word64BE,
  ) where

import           Control.Monad (replicateM)
import           Data.Bits
import qualified Data.ByteString as B
import           Data.Proxy
import           Data.Word
import Stream
import Machine


foldLE :: (Foldable t, Bits b, Integral b) => t b -> b
foldLE = foldr (\nxt acc -> nxt .|. shiftL acc 8) 0


foldBE :: (Foldable t, Bits b, Integral b) => t b -> b
foldBE = foldl (\acc nxt -> nxt .|. shiftL acc 8) 0


word8 :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word8
word8 = anyToken


-- Extract N bytes, expand them into a larger Integral value, then fold them
wordX :: (Monad m, Stream s, Word8 ~ StreamItem s, Bits a, Integral a)
  => Int -> ([a] -> a) -> MachineT s m a
wordX n f = f . expandWords <$> replicateM n word8
  where
    expandWords = fmap fromIntegral


-- | Select a 16 bit unsigned value, in appropriate byte order
word16LE, word16BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word16
word16LE = wordX 2 foldLE
word16BE = wordX 2 foldBE


-- | Select a 32 bit unsigned value, in appropriate byte order
word32LE, word32BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word32
word32LE = wordX 4 foldLE
word32BE = wordX 4 foldBE


-- | Select a 64 bit unsigned value, in appropriate byte order
word64LE, word64BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word64
word64LE = wordX 8 foldLE
word64BE = wordX 8 foldBE
