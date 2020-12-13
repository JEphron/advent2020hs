module Day11 where

-- import Control.Monad.Fix
import Data.Coerce
import qualified Data.List as List
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe

data Cell = Floor | Empty | Occupied deriving (Eq, Ord)

type Point = (Int, Int)

type Grid = Map Point Cell

showCell :: Cell -> String
showCell Floor = "."
showCell Empty = "L"
showCell Occupied = "#"

showGrid :: Grid -> String
showGrid =
  unlines
    . map (concat . map (showCell . snd))
    . List.groupBy (\((_, y), _) ((_, y2), _) -> y /= y2)
    . Map.toAscList

parseCell :: Char -> Cell
parseCell '.' = Floor
parseCell 'L' = Empty
parseCell '#' = Occupied

part1 :: [String] -> String
part1 =
  show
    . count Occupied
    . Map.elems
    . fixedPoint evolve
    . parseInput

evolve :: Grid -> Grid
evolve g =
  Map.mapWithKey (evolveCell g) g

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint fn x =
  let f = fn x
   in if f == x then x else fixedPoint fn f

evolveCell :: Grid -> Point -> Cell -> Cell
evolveCell grid point cell
  | cell == Floor = cell
  | n == 0 = Occupied
  | n >= 4 = Empty
  | otherwise = cell
  where
    n = count Occupied $ mapMaybe (grid !?) (adj point)

adj :: Point -> [Point]
adj p =
  [ up,
    down,
    left,
    right,
    up . left,
    up . right,
    down . left,
    down . right
  ]
    <*> [p]
  where
    up (x, y) = (x, y - 1)
    down (x, y) = (x, y + 1)
    left (x, y) = (x - 1, y)
    right (x, y) = (x + 1, y)

parseInput :: [String] -> Grid
parseInput ss =
  Map.fromList $ do
    (x, s) <- indexed ss
    (y, c) <- indexed s
    return ((x, y), parseCell c)

indexed =
  zip [0 ..]

count it =
  length . filter (== it)
