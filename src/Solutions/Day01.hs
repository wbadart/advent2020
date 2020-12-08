{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day01
  -- ( day01a
  -- , day01b
  -- ) where
where

import qualified Data.Set as S

day01a :: [String] -> Int
day01a = uncurry (*) . maybe (0, 0) run . traverse readMaybe

day01b :: [String] -> Int
day01b = maybe 0 (\(i, j, k) -> i * j * k) . viaNonEmpty head . maybe [] run' . traverse readMaybe

run :: [Int] -> (Int, Int)
run = either id (error "wat") . flip (evalStateT . traverse (go 2020)) S.empty

run' :: [Int] -> [(Int, Int, Int)]
run' xs = do
  i <- xs
  j <- xs
  let k = 2020 - (i + j)
  if k `elem` xs
     then return (i, j, k)
     else []

go :: Int -> Int -> StateT (Set Int) (Either (Int, Int)) ()
go sumTo i = do
  let j = sumTo - i
  cache <- get
  lift (if j `S.member` cache then Left (i, j) else Right ())
  modify (S.insert i)
  return ()
