{-# LANGUAGE OverloadedStrings,ViewPatterns #-}

module Solutions.Day05 where

import Control.Arrow ( (***) )

day05a :: NonEmpty String -> Either String Int
day05a = traverse seatId .> maximum1 >>> maybeToRight "bad parse"

day05b :: NonEmpty String -> Either String Int
day05b xs = do
  ids <- maybeToRight "bad parse" (traverse seatId xs >>= nonEmpty . sort . toList)
  let foo = evalState (traverse go ids) (prev (head ids))
  maybeToRight "no solution" $ viaNonEmpty (prev . head) $ fmap fst $ filter snd $ zip (toList ids) (toList foo)

go :: (Eq a, Enum a) => a -> State a Bool
go a = state \(succ -> a') -> (a /= a', a)

seatId :: String -> Maybe Int
seatId = splitAt 7 >>> (rowNum *** colNum) >>> uncurry (liftA2 (\r c -> r * 8 + c))
  where
    rowNum = solve (0, 127) 'F' 'B'
    colNum = solve (0, 7) 'L' 'R'
    solve :: (Int, Int) -> Char -> Char -> String -> Maybe Int
    solve initRange loChar hiChar = foldlM go initRange .> fst
      where
        go :: (Int, Int) -> Char -> Maybe (Int, Int)
        go (lo, hi) c | c == loChar = Just (lo, lo + ((hi - lo) `div` 2))
                      | c == hiChar = Just ((lo + (hi - lo) `div` 2) + 1, hi)
                      | otherwise   = Nothing
