{-# LANGUAGE OverloadedStrings #-}

module Main where

import Machine
import Stream
import Text

test1 :: Machine String Int
test1 = do
  text "hello" <|> text "goodbye" <?> "No prefix"
  (do
    some space
    length <$> sep (token ',') anyToken
    ) <|> return 0

showResult :: Show a => Either MachineFault (a, String) -> IO ()
showResult (Left err) = print err
showResult (Right (v, r)) = do
  print v
  putStrLn ("Unused:" ++ r)

main :: IO ()
main = do
  many $ do
    l <- getLine
    -- let rslt = runMachine naturalNumber l
    let rslt = runMachine (replicateMtoN 3 6 digit) l
    showResult rslt
  return ()
