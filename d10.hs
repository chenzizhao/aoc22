module Main where

data Op = AddX Int | Noop deriving (Show)

parse :: String -> [Op]
parse = concatMap f . lines
  where
    f s = case head s of
      'a' -> [Noop, AddX $ read $ drop 5 s]
      _ -> [Noop]

go :: Int -> Op -> Int
go acc Noop = acc
go acc (AddX x) = acc + x

task1 :: String -> Int
task1 s =
  let ops = parse s
      vals = scanl go 1 ops
      indices = 19 : [19 + 40 * i | i <- [1 ..]]
      indices' = takeWhile (< length vals) indices
   in sum $ map (\i -> (i+1) * vals !! i) indices'

render :: [Int] -> String
render l = foldl r "#" (zip l [1 .. 39])
    where r :: String -> (Int, Int) -> String
          r s (i, x) = s ++ if abs (i-x) <= 1 then "#" else "."

task2 :: String -> String
task2 s = 
    let vals = tail $ scanl go 1 $ parse s
        chunks = take 40 $ map (\i -> drop (i*40) vals) [0 .. length vals `div` 40]
     in unlines $ map render chunks

main :: IO ()
main = do
  s <- readFile "d10.txt"
  print $ task1 s
  putStrLn $ task2 s
  return ()