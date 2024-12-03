module Main where
import System.Environment
import Data.String.Utils

--import Text.Regex.Base.Context
import Text.Regex.Posix


--type MyMatchText source = Array Int (source, (MatchOffset, MatchLength))

main :: IO ()
main = do
  args <- getArgs
  basicData <- readFile (head args)
  fullData <- readFile (args !! 1)
  putStrLn ("basic: 161 ?= " ++ show (part1 basicData))
  putStrLn ("full: 161 ?= " ++ show (part1 fullData))


part1 :: String -> Int
part1 dataStr = sum ((product <$> map readInt) . tail <$> (\str -> str =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]) dataStr)

readInt :: String -> Int
readInt = read
