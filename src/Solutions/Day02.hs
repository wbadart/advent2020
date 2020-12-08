{-# LANGUAGE ViewPatterns #-}

module Solutions.Day02
  ( day02a
  , day02b
  ) where

day02a :: [String] -> Int
day02a = length . filter valid . mapMaybe parse

day02b :: [String] -> Int
day02b = length . filter valid' . mapMaybe parse

data Entry = Entry (Int, Int) Char String deriving Show

valid :: Entry -> Bool
valid (Entry (lo, hi) char password) =
  let count = filter (== char) password & length
   in count >= lo && count <= hi

valid' :: Entry -> Bool
valid' (Entry (lo, hi) char password) =
  maybe False id do
    lo' <- password !!? (lo - 1)
    hi' <- password !!? (hi - 1)
    return (xor (lo' == char) (hi' == char))

parse :: String -> Maybe Entry
parse
  (break (== ':') ->
    ( break (== ' ') ->
      ( break (== '-') -> (lo, '-':hi)
      , ' ':letter:[]
      )
    , ':':' ':password
    )
  )
    = Entry <$> ((,) <$> readMaybe lo <*> readMaybe hi) <*> pure letter <*> pure password
parse _ = Nothing
