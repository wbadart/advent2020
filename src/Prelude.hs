{-# LANGUAGE BlockArguments,ViewPatterns #-}

module Prelude
    ( module Relude
    , module Relude.Extra
    , (...)
    , (|>)
    , (.>)
    , interact
    , breakAll
    , split
    ) where

import System.IO ( getContents )

import Relude
import Relude.Extra

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = (>>>)

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
m .> f = fmap f . m

infixr 5 .>

interact :: (Text -> Text) -> IO ()
interact f = do
  blob <- getContents
  putTextLn $! f (toText $! blob)

breakAll :: Eq a => a -> [a] -> [[a]]
breakAll d = \case
  [] -> []
  (split d -> (l, r)) -> if null r then [l] else l : breakAll d r

split :: Eq a => a -> [a] -> ([a], [a])
split d (break (== d) -> (l, r)) =
  case r of
    d':r' -> (l, if d == d' then r' else r)
    _     -> (l, r)
