module Main where
import Data.List (elemIndex, sort)

computeCalories :: String -> [Int]
computeCalories = f.lines where
    f :: [String] -> [Int]
    f [] = []
    f xs = case elemIndex "" xs of
            Nothing -> [sumOfStrings xs]
            Just i -> sumOfStrings xs1 : f xs2 where
                xs1 = take i xs
                xs2 = drop (i+1) xs
            where sumOfStrings ss = sum $ map read ss

task1 :: String -> Int
task1 = maximum.computeCalories

task2 :: String -> Int
task2 = (sum <$> take 3) . reverse.sort <$> computeCalories

main :: IO ()
main = do
    f <- readFile "d1.txt"
    print $ task1 f
    print $ task2 f
