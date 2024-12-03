module Main where

import Data.List (nub)
import GHC.Arr (accum)

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

parse :: String -> [Char]
parse = concatMap ((\(c, i) -> replicate i c) . f) . lines
  where
    f s = (head s, read $ drop 2 s)

step :: (Coord, Coord) -> Char -> (Coord, Coord)
step (h, t) c =
  let h2 = case c of
        'R' -> Coord (x h + 1) (y h)
        'L' -> Coord (x h - 1) (y h)
        'U' -> Coord (x h) (y h + 1)
        'D' -> Coord (x h) (y h - 1)
      t2 = stepTail h2 t
   in (h2, t2)

stepTail :: Coord -> Coord -> Coord
stepTail h t =
  if abs (x h - x t) <= 1 && abs (y h - y t) <= 1
    then t
    else
      Coord
        (x t + (x h - x t) `safeDiv` abs (x h - x t))
        (y t + (y h - y t) `safeDiv` abs (y h - y t))
  where
    safeDiv :: Int -> Int -> Int
    safeDiv 0 0 = 0
    safeDiv _ 0 = 1
    safeDiv a b = a `div` b

task1 :: String -> Int
task1 s =
  let ls = parse s
      coords = scanl step (Coord 0 0, Coord 0 0) ls
      tailCoords = map snd coords
   in length $ nub tailCoords

step2 :: [Coord] -> Char -> [Coord]
step2 (h: rest) c =
  let h2 = case c of
        'R' -> Coord (x h + 1) (y h)
        'L' -> Coord (x h - 1) (y h)
        'U' -> Coord (x h) (y h + 1)
        'D' -> Coord (x h) (y h - 1)
   in h2 : stepN (h2: rest)
   where
     stepN :: [Coord] -> [Coord]
     stepN [h2] = []
     stepN (h2 : t : rest) = t2 : stepN (t2 : rest) where t2 = stepTail h2 t

task2 :: String -> Int
task2 s =
  let ls = parse s
      initCoords = [Coord 0 0 | _ <- [1 .. 10]]
      coords = scanl step2 initCoords ls
      tailCoords = map last coords  -- last != tail
   in length $ nub tailCoords

main :: IO ()
main = do
  f <- readFile "d9.txt"
  print $ task1 f
  print $ task2 f
