import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import System.IO

main :: IO ()
main = day7_2

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

runTest :: ([String] -> String) -> String -> IO ()
runTest fn inputFilename = do
  lines <- dayInput inputFilename
  putStrLn ""
  putStrLn "-----"
  putStrLn $ fn $ lines
  putStrLn "-----"

rootDir = "/home/jephron/dev/personal/advent2020/test/"

dayInput :: String -> IO [String]
dayInput inputFilename =
  lines <$> readFile (rootDir <> inputFilename)

getLines :: Handle -> IO [String]
getLines h = hGetContents h >>= return . lines
