{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.List (find, sort)
import Data.List.Split
import Data.Maybe

parseInput :: [String] -> (Int, [Int])
parseInput ss =
  let tsStr : busesStr : _ = ss
      ts = read tsStr :: Int
      buses = map read $ filter (/= "x") $ splitOn "," busesStr :: [Int]
   in (ts, buses)

part1 ss =
  let (ts, buses) = parseInput ss
      (ts', bus) =
        head $
          mapMaybe (\t -> (t,) <$> find (divisibleBy t) buses) $
            [(ts + 1) ..]
   in (ts' - ts) * bus

divisibleBy x y = x `mod` y == 0

part2 ss =
  {-
      # fuck this shit
      from sympy.ntheory.modular import crt
      import operator
      ms = [19, 41, 643, 17, 13, 23, 509, 37, 29]
      ns = [0, 9, 19, 36, 37, 42, 50, 56, 79]
      operator.sub(*reversed(crt(ms, ns)))
  -}
  ss
