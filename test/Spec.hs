{-# LANGUAGE BlockArguments,NumericUnderscores,OverloadedStrings #-}

import Text.Printf ( printf )

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( (@?=), assertBool, testCase )

import Solutions ( solnFunc )

main :: IO ()
main = do
  let tests =
        [ (01, 1_019_904,   176_647_680)
        , (02,       424,           747)
        , (03,       169, 7_560_370_818)
        , (04,       254,           184)
        , (05,       858,           557)
        , (06,     7_110,         3_628)
        , (07,       246,         2_976)
        , (08,     1_200,         1_023)
        ]
  input <- inputs (length tests)
  defaultMain $ testGroup "Advent of Code" (testDay input <$> tests)

inputs :: Int -> IO (Int -> Maybe (NonEmpty String))
inputs upto = do
  blobs <- traverse (readFile . printf "test/inputs/day%02d.txt") [1 :: Int .. upto]
  let inputLines = map (map toString . lines . toText) blobs
  pure ((inputLines !!?) >=> nonEmpty)

testDay :: (Int -> Maybe (NonEmpty String)) -> (Int, Int, Int) -> TestTree
testDay input (i, solnA, solnB) = testGroup (printf "Day %02d" i)
  let (implA, implB) = solnFunc i
      in_ = input (i - 1)
      missing = assertBool "input file not found" False
      case_ fmt impl soln = testCase (printf fmt i) $ maybe missing ((@?= Right soln) . impl) in_
   in [ case_ "day%02da" implA solnA, case_ "day%02db" implB solnB ]
