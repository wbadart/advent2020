{-# LANGUAGE BlockArguments #-}

module Prelude
    ( module Relude
    , module Relude.Extra
    , (...)
    , (|>)
    , (.>)
    , interact
    ) where

import System.IO ( hGetContents )

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
  blob <- hGetContents stdin
  putTextLn $! f (toText $! blob)
