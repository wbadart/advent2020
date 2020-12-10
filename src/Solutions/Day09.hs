{-# LANGUAGE MonadComprehensions #-}

module Solutions.Day09
  ( day09a
  , day09b
  ) where

import Data.Foldable ( minimum, maximum )
import Data.Sequence ( Seq ((:<|)), (|>) )
import qualified Data.Sequence as Seq

day09a :: NonEmpty String -> Either String Int
day09a = traverse readMaybe >>> maybeToRight "bad parse" >>> fmap toList >=> invalid 25

day09b :: NonEmpty String -> Either String Int
day09b =
  traverse readMaybe
  >>> maybeToRight "bad parse"
  >>> fmap toList
  >=> (id &&& invalid 25)
  >>> weakness


invalid :: Int -> [Int] -> Either String Int
invalid preambleSize = splitAt preambleSize >>> first Seq.fromList >>> go
  where
    go = \case
      (pre@(_ :<| rest), x:xs) ->
        if and [i + j /= x | i <- pre, j <- pre, j /= i]
           then Right x
           else go (rest |> x, xs)
      _ -> Left "no solution"

weakness :: ([Int], Either String Int) -> Either String Int
weakness = \case
  (_, Left e) -> Left e
  (xs, Right w) -> maybeToRight "no solution" $ viaNonEmpty head do
    set <- foldMap tails (inits xs)
    guard (not (null set) && sum set == w)
    return (minimum set + maximum set)
