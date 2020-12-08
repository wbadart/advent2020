{-# LANGUAGE OverloadedStrings #-}

module Solutions ( solnFunc ) where

import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04

solnFunc :: Int -> ([String] -> Int, [String] -> Int)
solnFunc = \case
  1 -> (day01a, day01b)
  2 -> (day02a, day02b)
  3 -> (day03a, day03b)
  4 -> (day04a, day04b)
  _ -> error "WIP"
