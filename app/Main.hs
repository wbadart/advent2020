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
  interact $ show . ((if secondProb then snd else fst) (solnFunc day)) . lines

solnFunc :: Int -> ([String] -> Int, [String] -> Int)
solnFunc = \case
  1 -> (day01a, day01b)
  2 -> (day02a, day02b)
  3 -> (day03a, day03b)
  4 -> (day04a, error "WIP")
  _ -> error "WIP"
