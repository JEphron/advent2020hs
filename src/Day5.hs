module Day5 where

import Data.Bifunctor
import Data.List

part1 :: [String] -> String
part1 = show . maximum . map seat

part2 :: [String] -> String
part2 =
  show
    . (+ 1)
    . firstOccurance (uncurry differsByOne)
    . pairwise
    . sort
    . map seat
  where
    firstOccurance by = fst . head . snd . span by
    differsByOne a b = (a + 1) == b

seat :: String -> Int
seat s =
  case rowCol s of
    (row, col) -> row * 8 + col

rowCol :: String -> (Int, Int)
rowCol =
  bimap (binPart 'B') (binPart 'R') . splitAt 7

binPart :: Char -> String -> Int
binPart one =
  binToDec . map (== one) . reverse

binToDec :: [Bool] -> Int
binToDec =
  ifoldl (\n i x -> n + (2 ^ i) * x) 0 . map fromEnum

-- utils

ifoldl :: (acc -> Int -> x -> acc) -> acc -> [x] -> acc
ifoldl fn a b =
  foldl (\acc (i, x) -> fn acc i x) a (zip [0 ..] b)

pairwise :: [a] -> [(a, a)]
pairwise list =
  zip list $ drop 1 list
