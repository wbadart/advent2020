module Solutions.Day02
  ( day02a
  , day02b
  ) where

day02a :: NonEmpty String -> Either String Int
day02a = solve valid

day02b :: NonEmpty String -> Either String Int
day02b = solve valid'

data Entry = Entry (Int, Int) Char String deriving Show

solve :: (Entry -> Bool) -> NonEmpty String -> Either String Int
solve p = traverse parseEntry >>> maybeToRight "bad parse" .> (toList >>> filter p >>> length)

valid :: Entry -> Bool
valid (Entry (lo, hi) c password) =
  let n = count (== c) password
   in n >= lo && n <= hi

valid' :: Entry -> Bool
valid' (Entry (lo, hi) c password) =
  fromMaybe False do
    lo' <- password !!? (lo - 1)
    hi' <- password !!? (hi - 1)
    return (xor (lo' == c) (hi' == c))

parseEntry :: String -> Maybe Entry
parseEntry
  (break (== ':') ->
    ( break (== ' ') ->
      ( break (== '-') -> (lo, '-':hi)
      , [' ', letter]
      )
    , ':':' ':password
    )
  )
    = Entry <$> ((,) <$> readMaybe lo <*> readMaybe hi) <*> pure letter <*> pure password
parseEntry _ = Nothing
