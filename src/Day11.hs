{-# LANGUAGE BangPatterns #-}

module Day11 where

-- import Control.Monad.Fix
import Data.Coerce
import qualified Data.List as List
import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace

data Cell = Floor | Empty | Occupied deriving (Eq, Ord, Show)

type Point = (Int, Int)

type Bounds = (Int, Int)

type Grid = Map Point Cell

type Rule = (Bounds -> Grid -> Point -> Cell -> Cell)

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
part1 = run rule1

part2 :: [String] -> String
part2 = run rule2

run :: Rule -> [String] -> String
run rule =
  show
    . count Occupied
    . Map.elems
    . fixedPoint (evolve rule)
    . parseInput

-- gixedPoint :: (Grid -> Grid) -> Grid -> Grid
-- gixedPoint fn x =
--   let f = fn x
--       !debug =
--         traceShowId $ count Occupied . Map.elems f
--    in if f == x then x else fixedPoint fn f

evolve :: Rule -> Grid -> Grid
evolve rule g =
  let !ggg = trace ("tick") ""
      bounds =
        gridBounds g
   in Map.mapWithKey (rule bounds g) g

rule1 :: Rule
rule1 bounds grid point cell
  | cell == Floor = cell
  | n == 0 = Occupied
  | n >= 4 = Empty
  | otherwise = cell
  where
    n = count Occupied $ mapMaybe (grid !?) (adj1 point)

adj1 :: Point -> [Point]
adj1 p =
  directions <*> [p]

rule2 :: Rule
rule2 bounds grid point cell
  | cell == Floor = cell
  | n == 0 = Occupied
  | n >= 5 = Empty
  | otherwise = cell
  where
    bleh = mapMaybe (grid !?) (adj2 bounds grid point)
    n = count Occupied $ bleh

adj2 :: Bounds -> Grid -> Point -> [Point]
adj2 bounds g p =
  catMaybes $ ((firstSeat bounds g) <$> directions) <*> [p]

firstSeat bounds g dir pt =
  List.find (\pp -> g ! pp /= Floor) $
    takeWhile (inBounds bounds) $ drop 1 $ iterate dir pt

inBounds bounds (x, y) =
  let (w, h) = bounds
   in x <= w && y <= h && x >= 0 && y >= 0

gridBounds :: Grid -> Bounds
gridBounds g =
  let keys = Map.keys g
   in (maximum $ fst <$> keys, maximum $ snd <$> keys)

gg = do
  let rootDir = "/home/jephron/dev/personal/advent2020/test/"
  gridStr <- lines <$> readFile (rootDir <> "day11.txt")
  return (parseInput gridStr)

directions :: [Point -> Point]
directions =
  [ up,
    down,
    left,
    right,
    up . left,
    up . right,
    down . left,
    down . right
  ]

up (x, y) = (x, y - 1)

down (x, y) = (x, y + 1)

left (x, y) = (x - 1, y)

right (x, y) = (x + 1, y)

parseInput :: [String] -> Grid
parseInput ss =
  Map.fromList $ do
    (y, s) <- indexed ss
    (x, c) <- indexed s
    return ((x, y), parseCell c)

indexed =
  zip [0 ..]

count it =
  length . filter (== it)

fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint fn x =
  let f = fn x
   in if f == x then x else fixedPoint fn f

headL :: [a] -> [a]
headL xs = if null xs then [] else [head xs]
