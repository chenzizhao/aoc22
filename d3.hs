import Data.Char (chr, isUpper, ord)
import Data.List (splitAt)
import Data.Set (Set, fromList, intersection, toList)

priority :: Char -> Int
priority c =
  if isUpper c
    then ord c - ord 'A' + 27
    else ord c - ord 'a' + 1

fromSingltonSet :: Set c -> c
fromSingltonSet = head . toList

f1 :: [Char] -> Char
f1 s =
  let (s1, s2) = splitAt (length s `div` 2) s
      (ss1, ss2) = (fromList s1, fromList s2)
   in fromSingltonSet $ intersection ss1 ss2

task1 :: String -> Int
task1 s = sum $ map (priority . f1) $ lines s

f2 :: [String] -> Int
f2 [] = 0
f2 ss = priority (g left) + f2 right
  where
    (left, right) = splitAt 3 ss
    -- point-free g?
    g ss = fromSingltonSet $ foldr1 intersection $ map fromList ss

task2 :: String -> Int
task2 = f2 . lines

main :: IO ()
main = do
  input <- readFile "d3.txt"
  print $ task1 input
  print $ task2 input