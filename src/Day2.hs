module Day2 where

import Data.List
import Data.List.Split

type Line = (Int, Int, Char, String)

parseLine :: String -> Line
parseLine lineString =
    let
        constraint : rest : [] = splitOn ": " lineString
        nums : pattern : [] = splitOn " " constraint
        a : b : [] = read <$> splitOn "-" nums
    in (a, b, head pattern, rest)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere fn = length . filter fn

run :: (Line -> Bool) -> [String] -> String
run fn = show . countWhere (fn . parseLine)

betwixt :: Int -> Int -> Int -> Bool
betwixt lo hi n = n >= lo && n <= hi

part1 :: [String] -> String
part1 = run (\(min, max, pattern, rest) ->
            betwixt min max $ countWhere (== pattern) rest)

part2 :: [String] -> String
part2 = run (\(a, b, pattern, rest) ->
            foldl1 (/=) $ (== pattern) . (!!) rest . pred <$> [a, b])
