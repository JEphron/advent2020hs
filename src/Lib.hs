module Lib
  ( someFunc,
    betwixt,
  )
where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

betwixt :: Int -> Int -> Int -> Bool
betwixt lo hi n = n >= lo && n <= hi
