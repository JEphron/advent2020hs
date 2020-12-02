module Day1 where

run :: [String] -> String
run strs = concat $ do
    let nums = map read strs
    a <- nums
    b <- nums
    c <- nums
    if a + b + c == 2020 then [show (a * b * c), "\n"] else []
