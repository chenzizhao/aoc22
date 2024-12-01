import Data.List (isPrefixOf, minimumBy)
import Data.Ord (comparing)
import System.IO ()

-- Define the data types
data FileSystem
  = File String Int -- A file with a name and size
  | Dir String [FileSystem] -- A directory with a name and contents
  deriving (Show, Eq)

-- Parse the file system trace
parseFileSystem :: [String] -> FileSystem
parseFileSystem = fst . go [] []
  where
    go :: [String] -> [FileSystem] -> [String] -> (FileSystem, [String])
    go path acc ("$ cd /" : rest) = go ["/"] [] rest
    go path acc ("$ ls" : rest) =
      let (contents, rest2) = break (isPrefixOf "$") rest
          parsedContents = map parseEntry contents
       in go path (acc ++ parsedContents) rest2
    go path acc (cmd : rest)
      | "$ cd " `isPrefixOf` cmd =
          let dirName = drop 5 cmd
           in if dirName == ".."
                then (Dir (last path) acc, rest)
                else
                  let (subDir, rest2) = go (path ++ [dirName]) [] rest
                   in go path (acc ++ [subDir]) rest2
    go path acc [] = (Dir (last path) acc, [])

    -- Parse individual entries (files or directories)
    parseEntry :: String -> FileSystem
    parseEntry line
      | "dir " `isPrefixOf` line = Dir (drop 4 line) []
      | otherwise =
          let (sizeStr, name) = break (== ' ') line
           in File (drop 1 name) (read sizeStr)

prettyPrint :: FileSystem -> String
prettyPrint = go 0
  where
    go indent (File name size) = replicate indent ' ' ++ name ++ " (" ++ show size ++ ")\n"
    go indent (Dir name contents) =
      replicate indent ' ' ++ name ++ "/\n" ++ concatMap (go (indent + 2)) contents

sizeOfFileSystem :: FileSystem -> Int
sizeOfFileSystem (File _ size) = size
sizeOfFileSystem (Dir _ contents) = sum $ map sizeOfFileSystem contents

allDirsUnderSizeN :: FileSystem -> Int -> [FileSystem]
allDirsUnderSizeN (File _ _) _ = []
allDirsUnderSizeN (Dir name contents) n =
  let size = sizeOfFileSystem (Dir name contents)
      rest = concatMap (`allDirsUnderSizeN` n) contents
   in if size <= n
        then Dir name contents : rest
        else rest

allDirsAboveSizeN :: FileSystem -> Int -> [FileSystem]
allDirsAboveSizeN (File _ _) _ = []
allDirsAboveSizeN (Dir name contents) n =
  let size = sizeOfFileSystem (Dir name contents)
      rest = concatMap (`allDirsAboveSizeN` n) contents
   in if size >= n
        then Dir name contents : rest
        else rest

task1 :: String -> Int
task1 content =
  let smallDirs = allDirsUnderSizeN (parseFileSystem $ lines content) 100000
   in sum $ map sizeOfFileSystem smallDirs

task2 :: String -> Int
task2 content =
  let fs = parseFileSystem $ lines content
      total = sizeOfFileSystem fs
      needN = 30000000 - (70000000 - total)
      bigDirs = allDirsAboveSizeN fs needN
      smallest = minimumBy (comparing sizeOfFileSystem) bigDirs
   in sizeOfFileSystem smallest

main :: IO ()
main = do
  content <- readFile "d7.txt"
  -- let commands = lines content
  -- let fileSystem = parseFileSystem commands
  -- putStrLn $ prettyPrint fileSystem
  print $ task1 content
  print $ task2 content