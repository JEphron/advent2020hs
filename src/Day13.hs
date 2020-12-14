{-# LANGUAGE TupleSections #-}

module Day13 where

import Data.List (find)
import Data.List.Split
import Data.Maybe

part1 ss =
  let tsStr : busesStr : _ = ss
      ts = read tsStr :: Int
      buses = map read $ filter (/= "x") $ splitOn "," busesStr :: [Int]
      (ts', bus) =
        head $
          mapMaybe (\t -> (t,) <$> find (divisibleBy t) buses) $
            [(ts + 1) ..]
   in show (ts, buses, ((ts' - ts) * bus))

divisibleBy x y = x `mod` y == 0
