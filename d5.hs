module Main where

import Data.List

parse1 :: String -> [Char]
parse1 s =
  let indices = map (\x -> 4 * x + 1) [0 .. 8]
      chars = map (s !!) indices
   in chars

data Move = Move {num :: Int, from :: Int, to :: Int} deriving (Show)

parse2 :: String -> Move
parse2 line =
  let [_, qty, _, src, _, dst] = words line
   in Move (read qty) (read src - 1) (read dst - 1)

applyMove :: Bool -> [[Char]] -> Move -> [[Char]]
applyMove keepOrder state move =
  let (fromRow, toRow) = (state !! from move, state !! to move)
      stack = take (num move) fromRow
      fromRow' = drop (num move) fromRow
      toRow' = (if keepOrder then stack else reverse stack) ++ toRow
   in [ if i == from move
          then fromRow'
          else
            if i == to move
              then toRow'
              else state !! i
        | i <- [0 .. 8]
      ]

safeHead :: [Char] -> Char
safeHead [] = 'e'
safeHead (x : _) = x

task :: Bool -> String -> [Char]
task keepOrder s =
  let state = transpose (map parse1 $ take 8 $ lines s)
      state' = map (filter (/= ' ')) state
      moves = map parse2 $ drop 10 $ lines s
      state'' = foldl (applyMove keepOrder) state' moves
   in map safeHead state''

main :: IO ()
main = do
  input <- readFile "d5.txt"
  print $ task False input
  print $ task True input