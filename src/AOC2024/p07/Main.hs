module Main where

import System.Environment

type Input = ()

parseInput :: FilePath -> IO Input
parseInput _path = pure ()

part1 :: Input -> Int
part1 = const 0

part2 :: Input -> Int
part2 = const 0

main :: IO ()
main = do
  [args] <- getArgs
  input <- parseInput args
  print $ part1 input
  print $ part2 input
