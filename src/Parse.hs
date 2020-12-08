{-# LANGUAGE DeriveFunctor,TupleSections #-}

module Parse where

import Data.Char
import Relude.Unsafe ( read )

newtype Parser i o = P { runParser :: i -> [(o, i)] } deriving Functor

evalParser :: Parser i o -> i -> Maybe o
evalParser = (fmap fst . viaNonEmpty head) ... runParser

instance Applicative (Parser i) where
  pure a = P $ pure . (a,)
  pf <*> po = P \i -> do
    (f, rest) <- runParser pf i
    first f <$> runParser po rest

instance Monad (Parser i) where
  return = pure
  p >>= f = P \i -> do
    (o, rest) <- runParser p i
    runParser (f o) rest

instance Alternative (Parser i) where
  empty = P (const empty)
  (P f1) <|> (P f2) = P \i -> case f1 i of [] -> f2 i; r  -> r

satisfying :: (a -> Bool) -> Parser [a] a
satisfying p = P \case
  (c : s) | p c -> [(c, s)]
  _             -> []

char :: Char -> Parser String Char
char c = satisfying (== c)

digit :: Parser String Int
digit = digitToInt <$> satisfying isDigit

number :: Parser String Int
number = read <$> some (satisfying isDigit)

signedNumber :: Parser String Int
signedNumber = (char '+' *> number) <|> (negate <$> (char '-' *> number))

string :: String -> Parser String String
string = \case
  "" -> pure ""
  (c : s) -> (:) <$> char c <*> string s

word :: Parser String String
word = some (satisfying isAlpha)

space :: Parser String Char
space = satisfying isSpace

spaces :: Parser String String
spaces = some space
