{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List
import System.Environment
import System.IO (readFile')

type Input = [[Int]]

parseInput :: FilePath -> IO Input
parseInput path = map (map read . words) . lines <$> readFile' path

isStrictlyMonotone :: [Int] -> Bool
isStrictlyMonotone levels = levels == sort levels || reverse levels == sort levels

maxLevelDifference :: [Int] -> Int
maxLevelDifference levels = maximum $ zipWith absDifference levels (tail levels)

minLevelDifference :: [Int] -> Int
minLevelDifference levels = minimum $ zipWith absDifference levels (tail levels)

absDifference :: Int -> Int -> Int
absDifference x y = abs $ x - y

isReportSafe :: [Int] -> Bool
isReportSafe report =
  isStrictlyMonotone report
    && maxLevelDifference report
    <= 3
    && minLevelDifference report
    >= 1

part1 :: Input -> Int
part1 = length . filter isReportSafe

derivedReports :: [Int] -> [[Int]]
derivedReports levels =
  levels
    : map
      ( \i ->
          map snd
            $ filter ((i /=) . fst)
            $ zip [0 ..] levels
      )
      [0 .. length levels - 1]

part2 :: Input -> Int
part2 = length . filter (any isReportSafe) . map derivedReports

main :: IO ()
main = do
  [args] <- getArgs
  input <- parseInput args
  print $ part1 input
  print $ part2 input
