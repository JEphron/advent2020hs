module Day3 where

steps = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

parseLine :: String -> [Int]
parseLine = cycle . map (fromEnum . (== '#'))

part2 :: [String] -> String
part2 strs =
  show $ product $ (countTrees $ parseLine <$> strs) <$> steps

countTrees :: [[Int]] -> (Int, Int) -> Int
countTrees lines (sx, sy) =
  snd $ foldl helper ((0, 0), 0) lines
  where
    helper ((x, y), t) line
      | y `divisibleBy` sy = ((x + sx, y + 1), t + line !! x)
      | otherwise = ((x, y + 1), t)

divisibleBy x y = x `mod` y == 0
