{-# LANGUAGE LambdaCase #-}

module Main where

import Criterion.Main
import Criterion.Main.Options
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Set qualified as S
import Data.Vector qualified as V
import Data.Word
import Options.Applicative
import System.Environment
import System.IO (readFile')

type Input = String

parseInput :: FilePath -> IO Input
parseInput = readFile'

part1 :: Input -> Int
part1 inp =
  maybe (-1) ((+ 4) . fst)
    $ find ((== 4) . length . snd)
    $ zip [0 ..]
    $ map (nub . take 4)
    $ tails inp

windows :: Int -> [a] -> [[a]]
windows windowSize =
  takeWhile ((== windowSize) . length)
    . map (take windowSize)
    . tails

windowedHOF :: (String -> Bool) -> String -> Int
windowedHOF isUniqueFn inp =
  maybe (-1) (+ 14)
    $ findIndex isUniqueFn
    $ windows 14 inp

windowAndSet :: Input -> Int
windowAndSet = windowedHOF ((== 14) . S.size . S.fromList)

windowAndList :: Input -> Int
windowAndList = windowedHOF ((== 14) . length . nub)

windowAndSetInsert :: Input -> Int
windowAndSetInsert =
  windowedHOF
    ( isJust
        . foldl'
          ( \mset c ->
              case mset of
                Nothing -> Nothing
                Just s
                  | S.member c s -> Nothing
                  | otherwise -> Just $ S.insert c s
          )
          (Just S.empty)
    )

windowAndListCons :: Input -> Int
windowAndListCons =
  windowedHOF
    ( isJust
        . foldl
          ( \mlist c ->
              case mlist of
                Nothing -> Nothing
                Just l
                  | c `elem` l -> Nothing
                  | otherwise -> Just $ c : l
          )
          (Just [])
    )

windowAndBits :: Input -> Int
windowAndBits inp =
  let first13 = take 13 inp
      initialState = foldl' (\s c -> s .^. (1 .<<. (ord c `mod` 32))) (0 :: Word32) first13
   in ( \case
          Right idx -> idx + 14
          Left _ -> -1
      )
        $ foldl'
          ( \s (i, w) ->
              case s of
                Right firstIdx -> Right firstIdx
                Left filter ->
                  let f = head w
                      l = last w
                      all14 = filter .^. (1 .<<. (ord l `mod` 32))
                      new13 = all14 .^. (1 .<<. (ord f `mod` 32))
                   in if popCount all14 == 14
                        then Right i
                        else Left new13
          )
          (Left initialState)
        $ zip [0 ..]
        $ windows 14 inp

noDuplicates :: (Eq a) => [a] -> Bool
noDuplicates = \case
  [] -> True
  (x : xs) -> x `notElem` xs && noDuplicates xs

recursionAndBits :: Input -> Int
recursionAndBits input =
  let inp = map ((1 .<<.) . (`mod` 32) . ord) input
      (first13, rest) = splitAt 13 inp
      initialState = foldl' (.^.) (0 :: Word32) first13
      go :: (Word32 -> (Word32, Word32) -> Maybe Word32) -> Word32 -> [(Word32, Word32)] -> Int
      go f = run 14
        where
          run !i _ [] = i
          run !i !s (x : xs) = case f s x of
            Nothing -> i
            Just !s' -> run (i + 1) s' xs
   in go
        ( \filter (f, l) ->
            let all14 = filter .^. l
                new13 = all14 .^. f
             in if popCount all14 == 14
                  then Nothing
                  else Just new13
        )
        initialState
        $ zip inp rest

recursionHOF :: (String -> Maybe Int) -> Input -> Int
recursionHOF findDuplicateOffsetFn =
  go 0
  where
    go _ [] = -1
    go !idx xss@(_ : xs) =
      let window = take 14 xss
          dupOffset = findDuplicateOffsetFn $ reverse window
       in case dupOffset of
            Nothing -> idx + 14
            Just offsetRev -> go (idx + 14 - offsetRev) (drop (13 - offsetRev) xs)

findDuplicateOffset :: (Ord a) => [a] -> Maybe Int
findDuplicateOffset = go 0 S.empty
  where
    go _ _ [] = Nothing
    go !offset !seen (x : xs)
      | S.member x seen = Just offset
      | otherwise = go (offset + 1) (S.insert x seen) xs

recursionAndSet :: Input -> Int
recursionAndSet = recursionHOF findDuplicateOffset

findDuplicateOffset' :: (Eq a) => [a] -> Maybe Int
findDuplicateOffset' = go 0 []
  where
    go _ _ [] = Nothing
    go offset !seen (x : xs)
      | x `elem` seen = Just offset
      | otherwise = go (offset + 1) (x : seen) xs

recursionAndList :: Input -> Int
recursionAndList = recursionHOF findDuplicateOffset'

findDuplicateOffset'' :: (Eq a) => [a] -> Maybe Int
findDuplicateOffset'' = go 0 (const False)
  where
    go _ _ [] = Nothing
    go offset seen (x : xs)
      | seen x = Just offset
      | otherwise = go (offset + 1) (\y -> y == x || seen y) xs

findDuplicateOffset''Vector :: (Eq a) => V.Vector a -> Maybe Int
findDuplicateOffset''Vector = go 0 (const False)
  where
    go offset seen v =
      case V.uncons v of
        Nothing -> Nothing
        Just (x, xs)
          | seen x -> Just offset
          | otherwise -> go (offset + 1) (\y -> y == x || seen y) xs

recursionAndIndexFunc :: Input -> Int
recursionAndIndexFunc = recursionHOF findDuplicateOffset''

vectorsRecursionHOF :: (V.Vector Char -> Maybe Int) -> Input -> Int
vectorsRecursionHOF findDuplicateOffsetFn = go 0 . V.fromList
  where
    go :: Int -> V.Vector Char -> Int
    go !idx cs =
      case V.uncons cs of
        Nothing -> -1
        Just (_, xs) ->
          let window = V.take 14 cs
              dupOffset = findDuplicateOffsetFn $ V.reverse window
           in case dupOffset of
                Nothing -> idx + 14
                Just offsetRev -> go (idx + 14 - offsetRev) (V.drop (13 - offsetRev) xs)

vectorsUnsafeRecursionHOF :: (V.Vector Char -> Maybe Int) -> Input -> Int
vectorsUnsafeRecursionHOF findDuplicateOffsetFn = go 0 . V.fromList
  where
    go :: Int -> V.Vector Char -> Int
    go !idx cs =
      case V.uncons cs of
        Nothing -> -1
        Just (_, xs) ->
          let window = V.unsafeTake 14 cs
              dupOffset = findDuplicateOffsetFn $ V.reverse window
           in case dupOffset of
                Nothing -> idx + 14
                Just offsetRev -> go (idx + 14 - offsetRev) (V.unsafeDrop (13 - offsetRev) xs)

recursionVectorsAndIndexFunc :: Input -> Int
recursionVectorsAndIndexFunc = vectorsRecursionHOF findDuplicateOffset''Vector

recursionVectorsUnsafeAndIndexFunc :: Input -> Int
recursionVectorsUnsafeAndIndexFunc = vectorsUnsafeRecursionHOF findDuplicateOffset''Vector

defaultMainWithArgs :: [String] -> [Benchmark] -> IO ()
defaultMainWithArgs args benchmarks = do
  wat <- handleParseResult $ execParserPure defaultPrefs (describe defaultConfig) args
  runMode wat benchmarks

main :: IO ()
main = do
  (inputPath : criterionArgs) <- getArgs
  input <- parseInput inputPath
  -- print $ part1 input
  print $ recursionVectorsUnsafeAndIndexFunc input

  defaultMainWithArgs
    criterionArgs
    [ env (parseInput inputPath) $ \ ~input ->
        bgroup
          "part2"
          [ bench "windowAndSet" $ nf windowAndSet input
          , bench "windowAndList" $ nf windowAndList input
          , bench "windowAndSetInsert" $ nf windowAndSetInsert input
          , bench "windowAndListCons" $ nf windowAndListCons input
          , bench "windowAndBits" $ nf windowAndBits input
          , bench "recursionAndBits" $ nf recursionAndBits input
          , bench "recursionAndSet" $ nf recursionAndSet input
          , bench "recursionAndList" $ nf recursionAndList input
          , bench "recursionAndIndexFunc" $ nf recursionAndIndexFunc input
          , bench "recursionVectorsAndIndexFunc" $ nf recursionVectorsAndIndexFunc input
          , bench "recursionVectorsUnsafeAndIndexFunc" $ nf recursionVectorsUnsafeAndIndexFunc input
          ]
    ]
