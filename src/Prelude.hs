{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,RankNTypes #-}

module Prelude
    ( module Relude
    , module Relude.Extra.Foldable1
    , module Relude.Extra.Tuple
    , module Lens.Micro
    , (...)
    , (.>)
    , breakAll
    , split
    , splice
    , count
    , Parser
    , number
    , signedNumber
    , word
    , spaces
    , getChar
    , parse
    , P.satisfy, P.char, P.string
    ) where

import Relude
import Relude.Extra.Foldable1
import Relude.Extra.Tuple
import Lens.Micro

import Data.Char
import Text.ParserCombinators.ReadP ( ReadP, readP_to_S, char, satisfy )
import qualified Text.ParserCombinators.ReadP as P
import Relude.Unsafe ( read )

-- ==========
-- Function Combinators
-- ==========

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
m .> f = fmap f . m

infixr 5 .>

-- ==========
-- List Munging
-- ==========

breakAll :: Eq a => a -> [a] -> [[a]]
breakAll d = \case
  [] -> []
  (split d -> (l, r)) -> if null r then [l] else l : breakAll d r

split :: Eq a => a -> [a] -> ([a], [a])
split d (break (== d) -> (l, r)) =
  case r of
    d':r' -> (l, if d == d' then r' else r)
    _     -> (l, r)

count :: (a -> Bool) -> [a] -> Int
count = length ... filter

splice :: Int -> a -> [a] -> [a]
splice i x (splitAt i -> (lhs, _:rhs)) = lhs <> (x:rhs)
splice _ _ xs = xs


-- ==========
-- Parsing
-- ==========

type Parser a = ReadP a

parse :: Parser a -> String -> Maybe a
parse = viaNonEmpty (fst . last) ... readP_to_S

signedNumber :: Parser Int
signedNumber = (negate <$> (char '-' *> number)) <|> (optional (char '+') *> number)

number :: Parser Int
number = read <$> some (satisfy isDigit)

word :: Parser String
word = some (satisfy isAlpha)

spaces :: Parser String
spaces = some (satisfy isSpace)

getChar :: Parser Char
getChar = P.get
