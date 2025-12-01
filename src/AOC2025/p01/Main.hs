module Main where

import System.Environment

data Rotation
  = RotateLeft Int
  | RotateRight Int

type Input = [Rotation]

parseInput :: FilePath -> IO Input
parseInput path = map readRotation . lines <$> readFile path
  where
    readRotation = \case
      'L' : distance -> RotateLeft $ read distance
      'R' : distance -> RotateRight $ read distance
      x -> error $ "Unknown rotation: " <> x

initialDialPosition :: Int
initialDialPosition = 50

part1 :: Input -> Int
part1 =
  length
    . filter (== 0)
    . snd
    . foldl'
      ( \(currentPosition, previousPositions) rot ->
          let newPosition = applyRotation currentPosition rot
           in (newPosition, newPosition : previousPositions)
      )
      (initialDialPosition, [])
  where
    applyRotation pos = \case
      RotateLeft dist -> (pos - dist) `mod` 100
      RotateRight dist -> (pos + dist) `mod` 100

part2 :: Input -> Int
part2 =
  snd
    . foldl'
      ( \(currentPosition, zeroCount) rot ->
          let (newPosition, newZeros) = applyRotation currentPosition rot
           in (newPosition, zeroCount + newZeros)
      )
      (initialDialPosition, 0)
  where
    applyRotation pos = \case
      RotateLeft dist -> ((pos - dist) `mod` 100, dist `div` 100 + if (pos - dist `mod` 100) <= 0 && pos > 0 then 1 else 0)
      RotateRight dist -> ((pos + dist) `mod` 100, dist `div` 100 + if (pos + dist `mod` 100 > 99) then 1 else 0)

main :: IO ()
main = do
  [args] <- getArgs
  input <- parseInput args
  print $ part1 input
  print $ part2 input
