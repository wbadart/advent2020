{-# LANGUAGE BlockArguments,NumericUnderscores,OverloadedStrings #-}

import Text.Printf ( printf )

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( (@?=), assertFailure, testCase )

import Solutions ( solnFunc )

main :: IO ()
main = do
  let tests =
        [ (01,   1_019_904,   176_647_680)
        , (02,         424,           747)
        , (03,         169, 7_560_370_818)
        , (04,         254,           184)
        , (05,         858,           557)
        , (06,       7_110,         3_628)
        , (07,         246,         2_976)
        , (08,       1_200,         1_023)
        , (09, 104_054_607,    13_935_797)
        , (10,       1_890,     undefined)
        ]
  input <- inputs (length tests)
  defaultMain $ testGroup "Advent of Code" (testDay input <$> tests)

inputs :: Int -> IO (Int -> Maybe (NonEmpty String))
inputs upto = do
  blobs <- traverse (readFile . printf "test/inputs/day%02d.txt") [1 :: Int .. upto]
  let inputLines = map (map toString . lines . toText) blobs
  pure ((inputLines !!?) >=> nonEmpty)

testDay :: (Int -> Maybe (NonEmpty String)) -> (Int, Int, Int) -> TestTree
testDay input (i, solnA, solnB) =
  let answers = do
        (implA, implB) <- solnFunc i    & maybeToRight "imlpementation not found"
        testInput      <- input (i - 1) & maybeToRight "input file not found"
        return $ (implA &&& implB) testInput
   in testGroup (printf "Day %02d" i) case answers of
        Left err -> [ testCase "bad" (assertFailure err) ]
        Right (a, b) -> [ testCase "A" (a @?= Right solnA), testCase "B" (b @?= Right solnB) ]
