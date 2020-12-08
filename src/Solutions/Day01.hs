{-# LANGUAGE OverloadedStrings,ViewPatterns #-}

module Solutions.Day01
  ( day01a
  , day01b
  ) where

import qualified Data.Set as S

day01a :: NonEmpty String -> Either String Int
day01a xs = do 
    ints <- maybeToRight "bad parse" (traverse readMaybe xs)
    uncurry (*) <$> run ints

day01b :: NonEmpty String -> Either String Int
day01b (toList -> xs) = do
  ints <- maybeToRight "bad parse" (traverse readMaybe xs)
  maybeToRight "no solution" $
    viaNonEmpty head [i * j * k | i <- ints, j <- ints, let k = 2020 - (i + j), k `elem` ints]

run :: NonEmpty Int -> Either String (Int, Int)
run = either Right (const $ Left "no solution") . flip (evalStateT . traverse go) S.empty
  where
    go :: Int -> StateT (Set Int) (Either (Int, Int)) ()
    go i = do
      let j = 2020 - i
      cache <- get
      lift (if j `S.member` cache then Left (i, j) else Right ())
      modify (S.insert i)
      return ()
