{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bifunctor
import Data.Char (isDigit)
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import System.Environment
import System.IO (readFile')

type Input = ([Int], [Int])

parseInput :: FilePath -> IO Input
parseInput path = parse <$> readFile' path
  where
    parse = unzip . map (bimap read read . span isDigit) . lines

distance :: (Num a) => a -> a -> a
distance x y = abs $ x - y

part1 :: Input -> Int
part1 = sum . uncurry (zipWith distance) . bimap sort sort

part2 :: Input -> Int
part2 (left, right) =
  let rightCounts =
        foldr
          ( M.alter
              ( \case
                  Just n -> Just $ n + 1
                  Nothing -> Just 1
              )
          )
          M.empty
          right
   in foldl (\score i -> fromMaybe 0 (M.lookup i rightCounts) * i + score) 0 left

main :: IO ()
main = do
  [args] <- getArgs
  input <- parseInput args
  print $ part1 input
  print $ part2 input
