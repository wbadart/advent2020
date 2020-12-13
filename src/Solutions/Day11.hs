{-# LANGUAGE RankNTypes #-}

module Solutions.Day11 where

import Prelude hiding ( prev )
import Data.Functor.Base ( ListF(..) )
import Data.Functor.Foldable ( cata )
import qualified Data.Map.Strict as M
import qualified Relude.Unsafe as Unsafe
import qualified Text.Show as TShow

day11a :: NonEmpty String -> Either String Int
day11a strings = do
  statuses <- traverse (traverse parsePos) strings & maybeToRight "bad parse"
  let evolution = iterate (applyRules 4 neighborsAdj) (toFerry $ toList statuses)
  zip evolution (Unsafe.tail evolution)
    & filter (uncurry (==))
    & viaNonEmpty (count occupied . M.elems . getLayout . fst . head)
    & maybeToRight "no solution"

day11b :: NonEmpty String -> Either String Int
day11b = undefined

applyRules :: Int -> ((Int, Int) -> (Int, Int) -> [(Int, Int)]) ->Ferry -> Ferry
applyRules tol neighbors (Ferry layout dim) = Ferry (M.mapWithKey go layout) dim
  where
    go p = \case
      Floor -> Floor
      Empty ->
        if not (any occupied' $ neighbors' p)
           then Occupied
           else Empty
      Occupied ->
        if count occupied' (neighbors' p) >= tol
           then Empty
           else Occupied
    occupied' = occupied . (layout M.!)
    neighbors' = neighbors dim

neighborsAdj :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighborsAdj (maxX, maxY) (x, y) =
  [ (x', y')
  | x' <- [0..maxX - 1], adjacent x' x
  , y' <- [0..maxY - 1], adjacent y' y
  , (x, y) /= (x', y')
  ]
  where adjacent a b = a == b || abs (a - b) == 1

occupied :: Status -> Bool
occupied = \case Occupied -> True; _ -> False

data Status = Floor | Empty | Occupied deriving Eq
instance Show Status where
  show = \case
    Floor -> "."
    Empty -> "L"
    Occupied -> "#"
parsePos :: Char -> Maybe Status
parsePos = \case
  '.' -> Just Floor
  'L' -> Just Empty
  '#' -> Just Occupied
  _   -> Nothing

data Ferry = Ferry { getLayout :: (Map (Int, Int) Status), getDim :: (Int, Int) } deriving Eq
instance Show Ferry where
  show (Ferry layout (dimX, dimY)) =
    let grid = replicate dimY ' ' <$ [1..dimX]
     in toString . unlines $ reverse (toText <$> M.foldlWithKey' go grid layout)
    where
      go :: [[Char]] -> (Int, Int) -> Status -> [[Char]]
      go grid (x, y) (Unsafe.head . Prelude.show -> status) = grid & ix x . ix y .~ status

toFerry :: [[Status]] -> Ferry
toFerry = cata \case
  Nil -> Ferry M.empty (0, 0)
  Cons row (Ferry layout (maxX, _)) ->
    let seats = zip [(maxX, j) | j <- [0..]] row
     in Ferry (M.union layout (M.fromList seats)) (maxX + 1, length row)

-- neighborsVisible :: Ferry -> (Int, Int) -> [(Int, Int)]
-- neighborsVisible (Ferry layout (maxX, maxY)) (x, y) = do
--   x' <- [0..maxX - 1]
--   y' <- [0..maxY - 1]
--   _
