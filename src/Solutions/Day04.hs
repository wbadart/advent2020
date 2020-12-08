{-# LANGUAGE ViewPatterns #-}

module Solutions.Day04
  ( day04a
  , day04b
  ) where

import Control.Arrow ( (***) )
import Data.Char ( isAlpha, isDigit, isHexDigit )
import Data.List ( lookup )
import qualified Data.Map.Strict as M

day04a :: [String] -> Int
day04a = solve (const True)

day04b :: [String] -> Int
day04b = solve and

solve :: ([Bool] -> Bool) -> [String] -> Int
solve f = length . filter id . map (maybe False f . valid . toPassport) . breakAll ""
  where
    toPassport = map (split ':') . concatMap (breakAll ' ')
    breakAll :: Eq a => a -> [a] -> [[a]]
    breakAll d = \case
      [] -> []
      (split d -> (l, r)) -> if null r then [l] else l : breakAll d r
    split :: Eq a => a -> [a] -> ([a], [a])
    split d (break (== d) -> (l, r)) =
      case r of
        d':r' -> (l, if d == d' then r' else r)
        _     -> (l, r)

valid :: [(String, String)] -> Maybe [Bool]
valid (filter ((/= "cid") . fst) -> fs) =
  traverse (uncurry (<*>) . ((fields M.!?) *** pure)) fs <* traverse (`Data.List.lookup` fs) (M.keys fields)

fields :: Map String (String -> Bool)
fields = (maybe False id .) <$> M.fromList
  [ ("byr", readMaybe .> between 1920 2002)
  , ("iyr", readMaybe .> between 2010 2020)
  , ("eyr", readMaybe .> between 2020 2030)
  , ("hgt", break isAlpha >>> (readMaybe *** checkHeight) >>> uncurry (>>=))
  , ("hcl", uncurry (liftA2 (&&)) . ((listToMaybe .> (== '#')) &&& (tailMay .> all isHexDigit)))
  , ("ecl", pure . (`elem` eyeColors))
  , ("pid", pure . ((&&) <$> ((== 9) . length) <*> all isDigit))
  ]
  where
    between lo hi d = d >= lo && d <= hi
    checkHeight = pure ... \case
      "cm" -> between 150 193
      "in" -> between  59  76
      _    -> const False
    eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    tailMay = \case [] -> Nothing; (_:xs) -> Just xs
    (...) = (.) . (.)
    m .> f = fmap f . m
