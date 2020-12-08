module Solutions.Day03
  ( day03a
  , day03b
  ) where

day03a :: NonEmpty String -> Either String Int
day03a = Right . solve 3 . toList

day03b :: NonEmpty String -> Either String Int
day03b ss = Right . product $ fmap ($ toList ss)
  [ solve 1
  , solve 3
  , solve 5
  , solve 7
  , solve 1 . mapMaybe (\(i, s) -> if even i then Just s else Nothing) . zip [0..]
  ]

go :: Int -> (Int, Int) -> String -> Maybe (Int, Int)
go right (r, i) s = s !!? i >>= \c -> pure (r + (if c == '#' then 1 else 0), i + right)

solve :: Int -> [String] -> Int
solve right = maybe 0 fst . foldlM (go right) (0, 0) . map cycle
