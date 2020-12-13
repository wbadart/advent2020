{-# LANGUAGE OverloadedStrings #-}

module Solutions.Day12 where

import qualified Relude.Unsafe as Unsafe

day12a :: NonEmpty String -> Either String Int
day12a strings = do
  route <- traverse parseNav strings & maybeToRight "bad parse"
  let (x, y) = last $ evalState (traverse solve route) (E, (0, 0))
  return (abs x + abs y)
  where
    solve :: (Nav, Int) -> State (Bearing, Position) Position
    solve (nav, n) = state \(direction, pos) ->
      case nav of
        L   -> (pos, (turn direction (abs (360 - n)), pos))
        R   -> (pos, (turn direction n,               pos))
        F   -> let pos' = move pos n direction in (pos', (direction, pos'))
        B d -> let pos' = move pos n d         in (pos', (direction, pos'))

day12b :: NonEmpty String -> Either String Int
day12b = undefined


turn :: Bearing -> Int -> Bearing
turn dir deg = iterate cycleNext dir Unsafe.!! (deg `div` 90)

move :: (Int, Int) -> Int -> Bearing -> (Int, Int)
move (x, y) n = \case
  N -> (x, y + n)
  E -> (x + n, y)
  S -> (x, y - n)
  W -> (x - n, y)

type Position = (Int, Int)
data Bearing = N | E | S | W deriving (Show, Bounded, Enum, Eq)
data Nav = B Bearing | F | L | R deriving Show
parseNav :: String -> Maybe (Nav, Int)
parseNav (splitAt 1 -> (Unsafe.head -> c, readMaybe -> Just n)) = do
  direction <-
    case c of
      'N' -> Just (B N)
      'S' -> Just (B S)
      'E' -> Just (B E)
      'W' -> Just (B W)
      'F' -> Just F
      'L' -> Just L
      'R' -> Just R
      _   -> error "bad parse"
  Just (direction, n)
parseNav _ = Nothing

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = if x == maxBound then minBound else succ x
