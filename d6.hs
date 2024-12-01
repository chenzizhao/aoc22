module Main where

task :: Int -> String -> Int
task l s = head [i | i <- [l - 1 .. length s-1], 
    allDifferent (take l (drop (i-l + 1) s))] + 1
  where
    allDifferent :: String -> Bool
    allDifferent [] = True
    allDifferent (x : xs) = x `notElem` xs && allDifferent xs


main :: IO ()
main = do
  input <- readFile "d6.txt"
  print $ task 4 input
  print $ task 14 input