module Day9 where

import Data.List
import Data.Maybe

windowed :: Int -> [a] -> [[a]]
windowed windowSize list =
  filter ((== windowSize) . length) $
    map
      (\i -> mapMaybe (getMaybe list) [i .. i + windowSize - 1])
      [0 .. length list]

getMaybe :: [a] -> Int -> Maybe a
getMaybe list index =
  if index >= length list
    then Nothing
    else Just $ list !! index

part1 :: [String] -> String
part1 s =
  show $ last <$> (find isLastNotSumOfPairs $ windowed 26 (map read s))

isLastNotSumOfPairs :: [Int] -> Bool
isLastNotSumOfPairs ints =
  not $ isSumOfPair (take (length ints - 1) ints) (last ints)

isSumOfPair :: [Int] -> Int -> Bool
isSumOfPair ints int =
  any (\a -> any (\b -> a + b == int) ints) ints

part2 :: [String] -> String
part2 s =
  show $ sumMinMax $ head $ mapMaybe (findContiguousAddies 20874512) (tails $ map read s)
  where
    findContiguousAddies target xs =
      find ((== target) . sum) $ inits xs

sumMinMax :: [Int] -> Int
sumMinMax x = minimum x + maximum x
