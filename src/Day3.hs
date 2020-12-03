module Day3 where

part1 :: [String] -> String
part1 =
  run [(3, 1)]

part2 :: [String] -> String
part2 =
  run [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

parseLine :: String -> [Bool]
parseLine = cycle . map (== '#')

run steps strs =
  show $ product $ (countTrees $ parseLine <$> strs) <$> steps

countTrees :: [[Bool]] -> (Int, Int) -> Int
countTrees lines (sx, sy) =
  snd $ foldl helper ((0, 0), 0) lines
  where
    helper ((x, y), t) line =
      if y `divisibleBy` sy
        then ((x + sx, y + 1), t + fromEnum (line !! x))
        else ((x, y + 1), t)

divisibleBy x y = x `mod` y == 0
