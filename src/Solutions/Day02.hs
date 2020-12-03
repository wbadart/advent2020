{-# LANGUAGE ViewPatterns #-}

module Solutions.Day02
  ( day02a
  , day02b
  ) where

import Data.Function

data Entry = Entry
  { range :: (Int, Int)
  , char :: Char
  , password :: String
  } deriving Show

day02a :: [String] -> Int
day02a = length . filter valid . map parse

day02b :: [String] -> Int
day02b = length . filter valid' . map parse

valid :: Entry -> Bool
valid (Entry (lo, hi) char password) =
  let count = filter (== char) password & length
   in count >= lo && count <= hi

valid' :: Entry -> Bool
valid' (Entry (lo, hi) char password) =
  let lo' = password !! (lo - 1) == char
      hi' = password !! (hi - 1) == char
   in xor lo' hi'
  where
    xor p q = (p || q) && not (p && q)

parse :: String -> Entry
parse
  (break (== ':') ->
    ( break (== ' ') ->
      ( break (== '-') -> (lo, '-':hi)
      , ' ':letter:[]
      )
    , ':':' ':password
    )
  )
    = Entry (read lo, read hi) letter password
-- parse (break (== ':') -> ((break (== ' ') -> (break (== '-') -> (lo, hi), letter:[]), ':':' ':password)) =
--   let (rangeStr, letter:[]) = break (== ' ') policyStr
--       (lo, hi) = break (== '-') rangeStr
--    in Entry (read lo, read hi) letter password
