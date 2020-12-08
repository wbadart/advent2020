{-# LANGUAGE ViewPatterns #-}

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
solve p = traverse parse |> maybeToRight "bad parse" .> toList |> filter p |> length

valid :: Entry -> Bool
valid (Entry (lo, hi) char password) =
  let count = filter (== char) password & length
   in count >= lo && count <= hi

valid' :: Entry -> Bool
valid' (Entry (lo, hi) char password) =
  fromMaybe False do
    lo' <- password !!? (lo - 1)
    hi' <- password !!? (hi - 1)
    return (xor (lo' == char) (hi' == char))

parse :: String -> Maybe Entry
parse
  (break (== ':') ->
    ( break (== ' ') ->
      ( break (== '-') -> (lo, '-':hi)
      , [' ', letter]
      )
    , ':':' ':password
    )
  )
    = Entry <$> ((,) <$> readMaybe lo <*> readMaybe hi) <*> pure letter <*> pure password
parse _ = Nothing
