{-# LANGUAGE NoImplicitPrelude,ViewPatterns #-}

module Solutions.Day05 where

import Relude
import Relude.Extra
import Control.Arrow ( (***) )

day05a :: [String] -> Maybe Int
day05a = viaNonEmpty maximum1 . map seatId

seatId :: String -> Int
seatId = splitAt 7 >>> (rowNum *** colNum) >>> uncurry (*)

rowNum :: String -> Int
rowNum = undefined

colNum :: String -> Int
colNum = undefined
