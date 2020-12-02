module Day2 where

import Data.List
import Data.List.Split

type Line = (Int, Int, Char, String)

split2 :: String -> String -> Maybe (String, String)
split2 needle haystack =
    case splitOn needle haystack of
        a:b:[] -> Just (a,b)
        _ -> Nothing

parseLine :: String -> Line
parseLine lineString =
    let
        constraint : rest : [] = splitOn ": " lineString
        nums : pattern : [] = splitOn " " constraint
        a : b : [] = read <$> splitOn "-" nums
    in (a, b, head pattern, rest)

countWhere :: (a -> Bool) -> [a] -> Int
countWhere fn = length . filter fn

part1 :: [String] -> String
part1 = show . countWhere (checkConstraint . parseLine)
    where
        checkConstraint (min, max, chr, rest) =
            let n = countWhere (== chr) rest
            in n >= min && n <= max

part2 :: [String] -> String
part2 = show . countWhere (checkPositionsMatch . parseLine)
    where
        checkPositionsMatch (a, b, pattern, rest) =
            (rest !! (a - 1) == pattern) /=  (rest !! (b - 1) == pattern)
