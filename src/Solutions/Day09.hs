{-# LANGUAGE MonadComprehensions #-}

module Solutions.Day09 where

import Data.Sequence ( Seq ((:<|)), (|>) )
import qualified Data.Sequence as Seq

day09a :: NonEmpty String -> Either String Int
day09a = traverse readMaybe >>> maybeToRight "bad parse" >>> fmap toList >=> solve 25
  where
    solve :: Int -> [Int] -> Either String Int
    solve preambleSize = splitAt preambleSize >>> first Seq.fromList >>> go
    go :: (Seq Int, [Int]) -> Either String Int
    go = \case
      (pre@(_ :<| rest), (x:xs)) ->
        if and [i + j /= x | i <- pre, j <- pre, j /= i]
           then Right x
           else go (rest |> x, xs)
      _ -> Left "no solution"

day09b :: NonEmpty String -> Either String Int
day09b = undefined
