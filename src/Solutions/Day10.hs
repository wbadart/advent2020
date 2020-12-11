module Solutions.Day10 where

import Prelude hiding ( prev )
import Data.Tree ( Tree(..) )
import Data.Functor.Base ( TreeF(..), NonEmptyF(..) )
import Data.Functor.Foldable

day10a :: NonEmpty String -> Either String Int
day10a =
  traverse readMaybe
  >>> maybeToRight "bad parse"
  >>> fmap
    (toList
    >>> sort
    >>> foldl' (\(prev :: Int, r) j -> (j, j - prev : r)) (0, [])
    >>> snd
    >>> count 1 &&& (+1) . count 3
    >>> uncurry (*))
  where
    count a = foldr (\x c -> if x == a then c + 1 else c) 0

day10b :: NonEmpty String -> Either String Int
day10b strings = do
  adapters <- traverse readMaybe strings & fmap (sort . toList) >>= nonEmpty & maybeToRight "bad parse"
  return $ nleaves (toTree adapters)

toTree :: NonEmpty Int -> Tree Int
toTree = cata \case
  NonEmptyF a Nothing -> Node a []
  NonEmptyF a (Just t@(Node _ kids)) -> Node a $ filter (rootLabel >>> connectsWith a) (t : kids)
  where
    a `connectsWith` b = abs (a - b) <= 3

nleaves :: Tree a -> Int
nleaves = cata \case NodeF _ ts -> maybe 1 sum (nonEmpty ts)
