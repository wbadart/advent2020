{-# LANGUAGE BlockArguments,DeriveAnyClass,DeriveGeneric,OverloadedStrings #-}

module Main where

import qualified Data.List as Old
import qualified System.IO as Old
import Options.Generic

import Solutions

data SolutionOpts = SolutionOpts
  { day :: Int
  , secondProb :: Bool
  } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
  SolutionOpts dayNum doSecond <- getRecord "WB Advent of Code"
  let soln = pick (not doSecond) <$> solnFunc dayNum
  input <- Old.lines <$> Old.getContents
  let result = do
        f   <- soln           & maybeToRight ("no solution for day " <> show dayNum)
        arg <- nonEmpty input & maybeToRight "empty input"
        f arg
  case result of
    Right answer -> print answer
    Left err -> do
      Old.hPutStrLn stderr ("ERROR: " <> err)
      exitFailure
