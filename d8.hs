module Main where

import GHC.Real (reduce)

parse :: String -> [[Int]]
parse = map (map charToInt) . lines
  where
    charToInt :: Char -> Int
    charToInt = read . return

viewable :: (Int, Int) -> [[Int]] -> Bool
viewable coord grid =
  any (\dir -> viewable' coord dir grid) [(-1, 0), (1, 0), (0, -1), (0, 1)]

viewable' :: (Int, Int) -> (Int, Int) -> [[Int]] -> Bool
viewable' (x, y) (dx, dy) grid = go (x + dx) (y + dy)
  where
    h = length grid
    w = length (head grid)
    val = grid !! x !! y
    go :: Int -> Int -> Bool
    go nx ny
      | nx < 0 || ny < 0 || nx >= h || ny >= w = True
      | grid !! nx !! ny >= val = False
      | otherwise = go (nx + dx) (ny + dy)

task1 :: String -> Int
task1 s =
  let grid = parse s
      grid' =
        [ viewable (x, y) grid
          | x <- [0 .. length grid - 1],
            y <- [0 .. length (head grid) - 1]
        ]
   in length $ filter id grid'

scenicScore :: (Int, Int) -> [[Int]] -> Int
scenicScore coord grid =
  product $
    map
      (\dir -> scenicScore' coord dir grid)
      [(1, 0), (-1, 0), (0, 1), (0, -1)]

scenicScore' :: (Int, Int) -> (Int, Int) -> [[Int]] -> Int
scenicScore' (x, y) (dx, dy) grid = go (x + dx) (y + dy) 1
  where
    h = length grid
    w = length (head grid)
    val = grid !! x !! y
    go :: Int -> Int -> Int -> Int
    go nx ny n
      | nx < 0 || ny < 0 || nx >= h || ny >= w = n - 1
      | grid !! nx !! ny >= val = n
      | otherwise = go (nx + dx) (ny + dy) (n + 1)

task2 :: String -> Int
task2 s =
  let grid = parse s
      grid' =
        [ [ scenicScore (x, y) grid
            | y <- [0 .. length (head grid) - 1]
          ]
          | x <- [0 .. length grid - 1] -- x is outer, and y is inner
        ]
   in maximum $ map maximum grid'

main :: IO ()
main = do
  f <- readFile "d8.txt"
  print $ task1 f
  print $ task2 f
