module Main where
import Data.Char (ord, chr)
import Text.Read (Lexeme(Char))

f1 :: String -> Int
f1 s = n1 + n2 where
    me = s!!2
    el = head s
    n1 = ord me - ord 'X' + 1
    n2 = 3 * ((ord me - ord 'A' - ord el + 1) `mod` 3)

f2 :: String -> Int
f2 s = n1 + n2 where
    goal = s!!2
    el = head s
    n1 = 3 * (ord goal - ord 'X')
    n2 = ((n1 `div` 3) - 1 + (ord el - ord 'A')) `mod` 3 + 1

task1 :: String -> Int
task1 s = sum $ map f1 $ lines s

task2 :: String -> Int
task2 s = sum $ map f2 $ lines s

main :: IO ()
main = do
    f <- readFile "d2.txt"
    print $ task1 f
    print $ task2 f
