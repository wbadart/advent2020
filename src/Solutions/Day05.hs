{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day05 where

import Control.Arrow ( (***) )

day05a :: NonEmpty String -> Either String Int
day05a = traverse seatId .> maximum1 >>> maybeToRight "bad parse"

day05b :: NonEmpty String -> Either String Int
day05b = undefined

seatId :: String -> Maybe Int
seatId = splitAt 7 >>> (rowNum *** colNum) >>> uncurry (liftA2 (\r c -> r * 8 + c))

rowNum :: String -> Maybe Int
rowNum = solve (0, 127) 'F' 'B'

colNum :: String -> Maybe Int
colNum = solve (0, 7) 'L' 'R'

solve :: (Int, Int) -> Char -> Char -> String -> Maybe Int
solve initRange loChar hiChar = foldlM go initRange .> fst
  where
    go :: (Int, Int) -> Char -> Maybe (Int, Int)
    go (lo, hi) c | c == loChar = Just (lo, lo + ((hi - lo) `div` 2))
                  | c == hiChar = Just ((lo + (hi - lo) `div` 2) + 1, hi)
                  | otherwise   = Nothing
