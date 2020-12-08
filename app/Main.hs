{-# LANGUAGE DeriveGeneric,OverloadedStrings #-}

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
  interact $ show . ((if secondProb then snd else fst) (solnFunc day)) . map toString . lines . toText
