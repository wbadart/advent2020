{-# LANGUAGE ScopedTypeVariables,ViewPatterns #-}

module Solutions.Day07
  ( day07a
  , day07b
  ) where

import qualified Data.Map.Strict as M

import Parse

day07a :: NonEmpty String -> Either String Int
day07a strings = do
  bags <- maybeToRight "bad parse" $ traverse (evalParser parseBag) strings
  let rules = M.fromList (toList bags)
  pure $ length $ filter (reachable "shinygold" rules) $ toList $ fmap fst bags

day07b :: NonEmpty String -> Either String Int
day07b strings = do
  bags <- maybeToRight "bad parse" $ traverse (evalParser parseBag) strings
  let rules = M.fromList (toList bags)
  pure $ sizeOfSubtree rules "shinygold"

type Bag = (String, Map String Int)
type Rules = Map String (Map String Int)

reachable :: String -> Rules -> String -> Bool
reachable needle rules ((rules M.!?) -> Just holds) =
  needle `M.member` holds || any (reachable needle rules) (M.keys holds)
reachable _ _ _ = False

sizeOfSubtree :: Rules -> String -> Int
sizeOfSubtree rules ((rules M.!?) -> Just holds) =
  sum holds
  + sum (M.toList holds & fmap (first (sizeOfSubtree rules) >>> uncurry (*)))
sizeOfSubtree _ _ = 0

parseBag :: Parser String Bag
parseBag = (\s1 s2 m -> (s1 <> s2, m))
  <$> word
  <* space
  <*> word
  <* string " bags contain "
  <*> parseSubBags

parseSubBags :: Parser String (Map String Int)
parseSubBags = either (const M.empty) M.fromList
  <$> (fmap Right (some parseSubBag)
  <|> fmap Left (string "no other bags."))

parseSubBag :: Parser String (String, Int)
parseSubBag = (\n s1 s2 -> (s1 <> s2, n))
  <$> number <* space
  <*> word <* space <*> word <* space
  <* string "bag" <* optional (string "s")
  <* (string "," <|> string ".")
  <* optional space
