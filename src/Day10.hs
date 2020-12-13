module Day10 where

import Data.List
import Data.Map ((!?))
import qualified Data.Map as Map

part1 :: [String] -> String
part1 s =
  let theDiffs = diffs $ parseInput s
   in show (count 1 theDiffs * count 3 theDiffs)

part2 :: [String] -> String
part2 = show . snd . fuck Map.empty . sort . parseInput

diffs :: [Int] -> [Int]
diffs = map (uncurry (flip (-))) . pairwise . sort

parseInput :: [String] -> [Int]
parseInput it =
  let input = map read it
   in ([0] ++ input ++ [maximum input + 3])

fuck m [] = (m, 0)
fuck m (_ : []) = (m, 1)
fuck m x =
  let guh x (mm, ns) =
        case m !? x of
          Just it ->
            (mm, it : ns)
          Nothing ->
            let (a, b) = fuck mm x
             in (Map.insert x b a, b : ns)

      lt3 y =
        head y - head x <= 3

      choices =
        if null x
          then []
          else tail $ takeWhile lt3 $ filter (not . null) $ tails x
   in sum <$> foldr guh (m, []) choices

-- utils --

pairwise :: [a] -> [(a, a)]
pairwise list =
  zip list $ drop 1 list

count it =
  length . filter (== it)
