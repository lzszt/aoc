{-# LANGUAGE OverloadedStrings #-}

module Problem01.Solution where

import Data.Char (isDigit)
import Data.Text qualified as T
import Debug.Trace

part1Input :: FilePath
part1Input = "./src/Problem1/part1_input.txt"

headTail :: [a] -> [a]
headTail xs = [head xs, last xs]

part1 :: IO ()
part1 = do
  inputLines <- lines <$> readFile part1Input
  let solution = sum $ map (read . headTail . filter isDigit) inputLines
  putStrLn $ "Problem 1, Part 1 Solution: " <> show @Int solution

part2Input :: FilePath
part2Input = "./src/Problem1/part2_input_test.txt"

replaceDigits :: String -> String
replaceDigits = T.unpack . (\t -> foldl (\t' (orig, rep) -> T.replace orig rep t') t replacements) . T.pack
  where
    replacements =
      [ ("one", "1")
      , ("two", "2")
      , ("three", "3")
      , ("four", "4")
      , ("five", "5")
      , ("six", "6")
      , ("seven", "7")
      , ("eight", "8")
      , ("nine", "9")
      ]

part2 :: IO ()
part2 = do
  inputLines <- lines <$> readFile part2Input
  let solution = sum $ traceShowId $ map (read . headTail . filter isDigit . replaceDigits) inputLines
  putStrLn $ "Problem 1, Part 2 Solution: " <> show @Int solution
