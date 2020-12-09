module Prelude
    ( module Relude
    , module Relude.Extra
    , (...)
    , (.>)
    , interact
    , breakAll
    , split
    , Parser
    , number
    , signedNumber
    , word
    , spaces
    , parse
    , P.satisfy, P.char, P.string
    ) where

import System.IO ( getContents )

import Relude
import Relude.Extra

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

interact :: (Text -> Text) -> IO ()
interact f = do
  blob <- getContents
  putTextLn $! f (toText $! blob)

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
