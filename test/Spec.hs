import System.IO
import qualified Day1


main :: IO ()
main = testDay1

testDay1 = do
    lines <- dayInput 1
    runTest lines Day1.run

runTest :: [String] -> ([String] -> String) -> IO ()
runTest lines fn = do
    putStrLn ""
    putStrLn "-----"
    putStrLn $ fn $ lines
    putStrLn "-----"

rootDir = "/home/jephron/dev/personal/advent2020/test/"

dayInput :: Int -> IO [String]
dayInput d = lines <$> readFile (rootDir <> "day" <> show d <> ".txt")

getLines :: Handle -> IO [String]
getLines h = hGetContents h >>= return . lines

