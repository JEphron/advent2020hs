import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import System.IO

main :: IO ()
main = day14_2

day14_2 = runTest Day14.part2 "day14.txt"

day14_1 = runTest Day14.part1 "day14.txt"

day13_2 = runTest Day13.part2 "day13.txt"

day13_1 = runTest Day13.part1 "day13.txt"

day12_1 = runTest Day12.part1 "day12.txt"

day11_2 = runTest Day11.part2 "day11.txt"

day11_1 = runTest Day11.part1 "day11.txt"

day10_2 = runTest Day10.part2 "day10.txt"

day10_1 = runTest Day10.part1 "day10.txt"

day9_2 = runTest Day9.part2 "day9.txt"

day9_1 = runTest Day9.part1 "day9.txt"

day8_2 = runTest Day8.part2 "day8.txt"

day8_1 = runTest Day8.part1 "day8.txt"

day7_2 = runTest Day7.part2 "day7.txt"

day7_1 = runTest Day7.part1 "day7.txt"

day6_2 = runTest Day6.part2 "day6.txt"

day6_1 = runTest Day6.part1 "day6.txt"

day5_2 = runTest Day5.part2 "day5.txt"

day5_1 = runTest Day5.part1 "day5.txt"

day4_1 = runTest Day4.part1 "day4.txt"

day3_2 = runTest Day3.part2 "day3.txt"

day3_1 = runTest Day3.part1 "day3.txt"

day2_2 = runTest Day2.part2 "day2.txt"

day2_1 = runTest Day2.part1 "day2.txt"

day1_2 = runTest Day1.run "day1.txt"

runTest :: Show a => ([String] -> a) -> String -> IO ()
runTest fn inputFilename = do
  lines <- dayInput inputFilename
  putStrLn ""
  putStrLn "-----"
  putStrLn $ show $ fn $ lines
  putStrLn "-----"

rootDir = "/home/jephron/dev/personal/advent2020/test/"

dayInput :: String -> IO [String]
dayInput inputFilename =
  lines <$> readFile (rootDir <> inputFilename)

getLines :: Handle -> IO [String]
getLines h = hGetContents h >>= return . lines
