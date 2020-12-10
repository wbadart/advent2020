{-# LANGUAGE ScopedTypeVariables #-}

module Solutions.Day10 where

day10a :: NonEmpty String -> Either String Int
day10a =
  traverse readMaybe
  >>> fmap (sort . toList)
  >>> maybeToRight "bad parse"
  >>> fmap
    (foldl' (\(prev :: Int, r) j -> (j, j - prev : r)) (0, [])
    >>> snd
    >>> count 1 &&& (+1) . count 3
    >>> uncurry (*))
  where
    count a = foldr (\x c -> if x == a then c + 1 else c) 0

day10b :: NonEmpty String -> Either String Int
day10b = undefined
