module Main where
import System.Environment
import System.Exit
import System.IO
import Control.Monad
import Control.Applicative
import Control.Exception
import Data.List

main :: IO ()
main = do
  args <- getArgs
  part1DataStr <- readFile (head args)
  putStrLn ("part 1 390 ?= " ++ show (part1 part1DataStr))
  -- not doing part 2 because it sucks

part1 :: String -> Int
part1 dataStr = do
  countBy goodDifferences (listDifference <$> tokenize dataStr)

listDifference :: [Int] -> [Int]
listDifference [] = []
listDifference [_] = []
listDifference (x:xs) = (head xs - x) : listDifference xs

goodDifferences :: [Int] -> Bool
goodDifferences diffs = all (\d -> d > 0 && d <= 3) diffs || all (\d -> d < 0 && d >= -3) diffs

countBy :: (a -> Bool) -> [a] -> Int
countBy pred list = length (filter pred list)

tokenize :: String -> [[Int]]
tokenize = map (map readInt <$> words) <$> lines

readInt :: String -> Int
readInt = read
