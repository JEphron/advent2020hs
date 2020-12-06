module Day6 where

import qualified Data.Char as Char
import Data.List.Split
import qualified Data.Set as Set

part1 :: [String] -> String
part1 =
  show . sum
    . map (length . Set.fromList . filter Char.isAlpha)
    . splitOn "\n\n"
    . unlines

part2 :: [String] -> String
part2 =
  show . sum
    . map (length . foldl1 Set.intersection . map Set.fromList . lines)
    . splitOn "\n\n"
    . unlines
