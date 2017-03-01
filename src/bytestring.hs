{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


foldLE, foldBE :: (Foldable t, Bits b, Integral b) => t b -> b
foldLE = foldr (\nxt acc -> nxt .|. shiftL acc 8) 0
foldBE = foldl (\acc nxt -> nxt .|. shiftL acc 8) 0


word8 :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word8
word8 = anyToken


-- Extract N bytes, expand them into a larger Integral value, then fold them
wordX :: (Monad m, Stream s, Word8 ~ StreamItem s, FiniteBits a, Integral a)
  => a -> ([a] -> a) -> MachineT s m a
wordX undefRslt f =
  f . fmap fromIntegral <$>
  replicateM (finiteBitSize undefRslt `div` 8) word8


-- | Select a 16 bit unsigned value, in appropriate byte order
word16LE, word16BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word16
word16LE = wordX (undefined :: Word16) foldLE
word16BE = wordX (undefined :: Word16) foldBE


-- | Select a 32 bit unsigned value, in appropriate byte order
word32LE, word32BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word32
word32LE = wordX (undefined :: Word32) foldLE
word32BE = wordX (undefined :: Word32) foldBE


-- | Select a 64 bit unsigned value, in appropriate byte order
word64LE, word64BE :: (Monad m, Stream s, Word8 ~ StreamItem s) => MachineT s m Word64
word64LE = wordX (undefined :: Word64) foldLE
word64BE = wordX (undefined :: Word64) foldBE
