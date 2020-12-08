{-# LANGUAGE NoImplicitPrelude,OverloadedStrings,ViewPatterns #-}

module Solutions.Day05 where

import Relude
import Relude.Extra
import Control.Arrow ( (***) )

day05a :: NonEmpty String -> Either String Int
day05a = Right . maximum1 . fmap seatId

day05b :: NonEmpty String -> Either String Int
day05b = undefined

seatId :: String -> Int
seatId = splitAt 7 >>> (rowNum *** colNum) >>> uncurry (*)

rowNum :: String -> Int
rowNum = undefined

colNum :: String -> Int
colNum = undefined
