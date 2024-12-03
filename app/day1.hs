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
  part2DataStr <- readFile (head args)
  putStrLn ("part 1 2970687 ?= " ++ show (part1 part1DataStr))
  putStrLn ("part 2 23963899 ?= " ++ show (part2 part1DataStr))

part1 :: String -> Int
part1 dataStr = do
  let part1Tokens = tokenize dataStr
  ((sum <$> uncurry zipAbsDiff) . ap2 sort <$> unzip) part1Tokens

part2 :: String -> Int
part2 dataStr = do
  let tokens = tokenize dataStr
  uncurry part2Similarity <$> unzip $ tokens

part2Similarity :: [Int] -> [Int] -> Int
part2Similarity [] _ = 0
part2Similarity (x:xs) y = x * length (elemIndices x y) + part2Similarity xs y

readInt :: String -> Int
readInt = read

tokenize :: String -> [(Int, Int)]
tokenize = map (tuple2 . map readInt <$> words) <$> lines

tuple2 :: [a] -> (a, a)
tuple2 [x, y] = (x, y)

ap2 :: (a -> b) -> (a, a) -> (b, b)
ap2 f (x, y) = (f x, f y)

zipAbsDiff :: [Int] -> [Int] -> [Int]
zipAbsDiff = zipWith ((abs .) . (-))
