{-# LANGUAGE RecordWildCards #-}

module Problem03.Solution where

import Data.Char (isDigit)
import Data.Foldable
import Data.Function
import Data.List.Split
import Debug.Trace

data Cell = Cell
  { cellChar :: Char
  , cellSymbolAdjacent :: Bool
  }
  deriving (Eq)

initialCell :: Char -> Cell
initialCell c =
  Cell
    { cellChar = c
    , cellSymbolAdjacent = isSymbol c
    }

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c)

type Input = [[Cell]]

mapInput :: (Cell -> Cell) -> Input -> Input
mapInput f = map (map f)

toString :: Input -> String
toString = concatMap (map cellChar)

prettyInput :: Input -> String
prettyInput i =
  unlines $
    map
      ( concatMap
          ( \Cell{..} ->
              ( if cellSymbolAdjacent
                  then "\ESC[31m"
                  else "\ESC[0m"
              )
                <> [cellChar]
          )
      )
      i
      <> ["\ESC[0m"]

printInput :: Input -> IO ()
printInput = putStrLn . prettyInput

type Coords = (Int, Int)

readInput :: FilePath -> IO Input
readInput file =
  map (map initialCell) . lines <$> readFile file

getCell :: Input -> Coords -> Cell
getCell inp (r, c) = inp !! r !! c

hasSymbolNeighbour :: Input -> Coords -> Bool
hasSymbolNeighbour inp (r, c) = any (cellSymbolAdjacent . getCell inp) neighbourIndices
  where
    neighbourIndices =
      [ (r + dR, c + dC)
      | dR <- [-1, 0, 1]
      , r + dR >= 0
      , r + dR < nRows
      , dC <- [-1, 0, 1]
      , c + dC >= 0
      , c + dC < nCols
      ]
    nRows = length inp
    nCols = length $ head inp

updateCell :: Coords -> (Cell -> Cell) -> Input -> Input
updateCell (r, c) trans =
  mapIndexed
    ( \rI ->
        if rI == r
          then
            mapIndexed
              ( \cI cell ->
                  if cI == c
                    then trans cell
                    else cell
              )
          else id
    )

mapIndexed :: (Num a, Enum a) => (a -> b -> c) -> [b] -> [c]
mapIndexed f = zipWith f [0 ..]

propagateSymbols :: Input -> Input
propagateSymbols i = foldl' (\inp coords -> updateCell coords (trans coords) inp) i indicies
  where
    trans coords cell
      | isDigit $ cellChar cell = cell{cellSymbolAdjacent = hasSymbolNeighbour i coords}
      | otherwise = cell
    indicies = [(r, c) | r <- [0 .. nRows - 1], c <- [0 .. nCols - 1]]
    nRows = length i
    nCols = length $ head i

replaceInvalid :: Cell -> Cell
replaceInvalid cell@Cell{..} = cell{cellChar = if cellSymbolAdjacent && not (isSymbol cellChar) then cellChar else '.'}

part1 :: IO ()
part1 = do
  inp <- readInput "./src/Problem03/part1_input.txt"
  let propagatedSymbols =
        fix
          ( \rec i ->
              let newInp = propagateSymbols i
               in trace (prettyInput newInp) $
                    if newInp == i
                      then i
                      else rec newInp
          )
          inp
      noSymbols = mapInput replaceInvalid propagatedSymbols
  print $ sum $ map (read @Int) $ filter (not . null) $ splitOn "." $ toString noSymbols
