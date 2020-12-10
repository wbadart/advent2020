{-# LANGUAGE OverloadedStrings #-}

module Solutions ( Solution, solnFunc, pick ) where

import Solutions.Day01
import Solutions.Day02
import Solutions.Day03
import Solutions.Day04
import Solutions.Day05
import Solutions.Day06
import Solutions.Day07
import Solutions.Day08
import Solutions.Day09
import Solutions.Day10

type Solution = NonEmpty String -> Either String Int

solnFunc :: Int -> Maybe (Solution, Solution)
solnFunc = \case
  01 -> Just (day01a, day01b)
  02 -> Just (day02a, day02b)
  03 -> Just (day03a, day03b)
  04 -> Just (day04a, day04b)
  05 -> Just (day05a, day05b)
  06 -> Just (day06a, day06b)
  07 -> Just (day07a, day07b)
  08 -> Just (day08a, day08b)
  09 -> Just (day09a, day09b)
  10 -> Just (day10a, day10b)
  _  -> Nothing

pick :: Bool -> (a, a) -> a
pick p (a1, a2) = if p then a1 else a2
