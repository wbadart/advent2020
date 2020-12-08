module Solutions.Day06
  ( day06a
  , day06b
  ) where

import qualified Data.Set as S

day06a :: NonEmpty String -> Either String Int
day06a = solve $ unstableNub . mconcat

day06b :: NonEmpty String -> Either String Int
day06b = solve $ foldl1' S.intersection . maybe (pure S.empty) (fmap S.fromList) . nonEmpty

solve :: Foldable f => ([String] -> f a) -> NonEmpty String -> Either String Int
solve f = Right . sum . map (length . f) . breakAll "" . toList
