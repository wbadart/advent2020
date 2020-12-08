{-# LANGUAGE OverloadedStrings #-}

module Solutions ( solnFunc ) where

import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07

type Solution = NonEmpty String -> Either String Int

solnFunc :: Int -> (Solution, Solution)
solnFunc = \case
  1 -> (day01a, day01b)
  2 -> (day02a, day02b)
  3 -> (day03a, day03b)
  4 -> (day04a, day04b)
  5 -> (day05a, day05b)
  6 -> (day06a, day06b)
  7 -> (day07a, day07b)
  _ -> error "WIP"
