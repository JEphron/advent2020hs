{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

part1 (tsStr : busesStr : _) =
  let ts = read tsStr
      buses = mapMaybe readMaybe $ splitOn "," busesStr
      divisibleBy x y = x `mod` y == 0
      findBus = flip find buses
      findDivisibleBy t = (t,) <$> findBus (divisibleBy t)
      (ts', bus) : _ = mapMaybe (findDivisibleBy) $ [ts + 1 ..]
   in (ts' - ts) * bus

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
