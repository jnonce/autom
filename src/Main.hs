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

main :: IO ()
main = do
  many $ do
    l <- getLine
    let rslt = runMachine test1 l
    case rslt of
      Left err -> print err
      Right (v, b) -> print v >> putStrLn ("left: " ++ b)
  return ()
