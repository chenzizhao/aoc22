import Data.Maybe (fromJust)
import Data.List (elemIndex)

splitAt' :: Int -> String -> (String, String)
splitAt' i s = (take i s, drop (i+1) s)

parse :: String -> ((Int, Int), (Int, Int))
parse s = (parse' p1, parse' p2) where
  piv = fromJust $ elemIndex ',' s
  (p1, p2) = splitAt' piv s
  parse' :: String -> (Int, Int)
  parse' s' = (read left, read right) where
    piv = fromJust $ elemIndex '-' s'
    (left, right) = splitAt' piv s'

f1 :: ((Int, Int), (Int, Int)) -> Bool 
f1 ((a1, b1), (a2, b2)) = p1 || p2 where
  p1 = (a1 <= a2) && (b1 >= b2)
  p2 = (a2 <= a1) && (b2 >= b1)

f2 :: ((Int, Int), (Int, Int)) -> Bool 
f2 ((a1, b1), (a2, b2)) = p1 || p2 where
  p1 = (a2 <= a1) && (a1 <= b2)
  p2 = (a1 <= a2) && (a2 <= b1)

task1 :: String -> Int
task1 s = (length.filter id) $ map (f1.parse) $ lines s

task2 :: String -> Int
task2 s = (length.filter id) $ map (f2.parse) $ lines s

main :: IO ()
main = do
  input <- readFile "d4.txt"
  print $ task1 input
  print $ task2 input