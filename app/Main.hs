{-# LANGUAGE BlockArguments,DeriveGeneric,OverloadedStrings #-}

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
  interact
    $ lines
    >>> map toString
    >>> nonEmpty
    >>> (maybeToRight "empty input" >=> (bool fst snd secondProb $ solnFunc day))
    >>> either (toText . ("ERROR " <>)) show
