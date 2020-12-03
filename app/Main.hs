{-# LANGUAGE DeriveGeneric,LambdaCase,OverloadedStrings #-}

module Main where

import Options.Generic
import Solutions

data Solution = Solution
  { day :: Int
  , secondProb :: Bool
  } deriving (Show, Generic)
instance ParseRecord Solution

main :: IO ()
main = do
  Solution day secondProb <- getRecord "WB Advent of Code"
  interact $ show . solnFunc (day, secondProb) . lines

solnFunc :: (Int, Bool) -> ([String] -> Int)
solnFunc = \case
  (1, False) -> day01a
  (1, True)  -> day01b
  (2, False) -> day02a
  (2, True)  -> day02b
  _ -> error "WIP"
