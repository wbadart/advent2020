{-# LANGUAGE LambdaCase,ScopedTypeVariables,ViewPatterns #-}

module Solutions.Day04 where
  -- ( day04a
  -- -- , day04b
  -- ) where

import Data.Maybe ( mapMaybe )

day04a :: [String] -> Int
day04a = length . mapMaybe (valid . toPassport) . breakAll ""

toPassport :: [String] -> [(String, String)]
toPassport = map (split ':') . concatMap (breakAll ' ')

valid :: [(String, String)] -> Maybe ()
valid entries = () <$ traverse (`lookup` entries) fields
  where
    fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]  -- "cid"

breakAll :: Eq a => a -> [a] -> [[a]]
breakAll d = \case
  [] -> []
  (break (== d) -> (l, r)) ->
    case r of
      d':r' -> l : breakAll d (if d == d' then r' else r)
      _    -> [l]

split :: Eq a => a -> [a] -> ([a], [a])
split d (break (== d) -> (l, r)) =
  case r of
    d':r' -> (l, if d == d' then r' else r)
    _     -> (l, r)
