{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import           Data.Word
import Machine
import Stream
import Text
import ByteString

test1 :: Machine String Int
test1 = do
  text "hello" <|> text "goodbye" <?> "No prefix"
  (do
    some space
    length <$> sep (token ',') anyToken
    ) <|> return 0

arrayB = B.pack [127, 64, 0, 128, 255]

showResult :: (Show a, Show r) => Either MachineFault (a, r) -> IO ()
showResult (Left err) = print err
showResult (Right (v, r)) = do
  print v
  print r

main :: IO ()
main = do
  many $ do
    -- l <- B.getLine
    -- let rslt = runMachine naturalNumber l
    -- let rslt = runMachine (replicateMtoN 3 6 digit) l
    let rslt = runMachine int32LE arrayB
    showResult rslt
  return ()
