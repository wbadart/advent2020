{-# LANGUAGE BlockArguments #-}

module Prelude
    ( module Relude
    , module Relude.Extra
    , (...)
    , (.>)
    , interact
    ) where

import Relude
import Relude.Extra

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

(.>) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
m .> f = fmap f . m

interact :: (Text -> Text) -> IO ()
interact f = forever do
  line <- getLine
  putStrLn $! toString (f line)
