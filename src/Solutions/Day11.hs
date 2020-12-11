module Solutions.Day11 where

import Data.Functor.Base ( ListF(..) )
import Data.Functor.Foldable ( cata )
import qualified Data.Map.Strict as M
import qualified Relude.Unsafe as Unsafe
import Text.Show ( Show(..) )

day11a :: NonEmpty String -> Either String Int
day11a strings = do
  statuses <- traverse (traverse parsePos) strings & maybeToRight "bad parse"
  let evolution = iterate applyRules $ toFerry (toList statuses)
      withPrev = zip evolution (Unsafe.tail evolution)
  fmap (length . filter occupied) $ maybeToRight "no solution" $ viaNonEmpty head do
    (prev, f@(Ferry layout _)) <- withPrev
    if prev == f
       then return (M.elems layout)
       else []

data Status = Floor | Empty | Occupied deriving Eq
instance Show Status where
  show = \case
    Floor -> "."
    Empty -> "L"
    Occupied -> "#"

data Ferry = Ferry (Map (Int, Int) Status) (Int, Int) deriving Eq

f :: IO Ferry
f = do
  blob <- fmap toString . lines <$> readFileText "test.txt"
  let Just f = toFerry <$> traverse (traverse parsePos) blob
  return f

instance Show Ferry where
  show (Ferry layout (dimX, dimY)) =
    let grid = const (replicate dimY ' ') <$> [1..dimX]
     in toString $ unlines $ reverse $ fmap toText $ M.foldlWithKey' go grid layout
    where
      go :: [[Char]] -> (Int, Int) -> Status -> [[Char]]
      go grid (x, y) (Unsafe.head . Prelude.show -> status) =
        let row = grid ^. idx x
            row' = set (idx y) status row
         in set (idx x) row' grid

parsePos :: Char -> Maybe Status
parsePos = \case
  '.' -> Just Floor
  'L' -> Just Empty
  '#' -> Just Occupied
  _   -> Nothing

toFerry :: [[Status]] -> Ferry
toFerry = cata \case
  Nil -> Ferry M.empty (0, 0)
  Cons row (Ferry layout (maxX, _)) ->
    let seats = zip [(maxX, j) | j <- [0..]] row
     in Ferry (M.union layout (M.fromList seats)) (maxX + 1, length row)

applyRules :: Ferry -> Ferry
applyRules f@(Ferry layout dim@(maxX, maxY)) = Ferry (M.mapWithKey go layout) dim
  where
    go p = \case
      Floor -> Floor
      Empty ->
        if (not . occupied') `all` (neighbors p)
           then Occupied
           else Empty
      Occupied ->
        if length (filter occupied' (neighbors p)) >= 4
           then Empty
           else Occupied
    neighbors (x, y) =
      [ (x', y') | x' <- [0..maxX - 1], y' <- [0..maxY - 1]
      , adjacent x' x, adjacent y' y, (x, y) /= (x', y')
      ]
    adjacent a b = a == b || a == b + 1 || a == b - 1
    occupied' = occupied . (layout M.!)

occupied = \case Occupied -> True; _ -> False


day11b :: NonEmpty String -> Either String Int
day11b = undefined
