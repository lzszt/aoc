{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.Char (isNumber)
import Data.List.Split
import Data.Map.Strict qualified as M
import Debug.Trace
import System.Environment

data Blueprint
  = Blueprint
  { blueprintId :: Int
  , oreRobotCost :: Int
  , clayRobotCost :: Int
  , obsidianRobotCost :: (Int, Int)
  , geodeRobotCost :: (Int, Int)
  }

type Input = [Blueprint]

parseInput :: FilePath -> IO Input
parseInput =
  fmap (map parseBlueprint . lines)
    . readFile
  where
    parseBlueprint bp = case filter (not . null) $ map (filter isNumber) $ splitOn " " bp of
      [a, b, c, d, e, f, g] ->
        Blueprint
          { blueprintId = read a
          , oreRobotCost = (read b)
          , clayRobotCost = (read c)
          , obsidianRobotCost = (read d, read e)
          , geodeRobotCost = (read f, read g)
          }
      x -> error $ "Failed to parse blueprint " <> show x

timeBudget :: Int
timeBudget = 24

blueprintQuality :: Blueprint -> Int
blueprintQuality bp =
  bp.blueprintId * maximumNumberOfGeodes bp

canBuildGeodeRobot :: Blueprint -> State -> Bool
canBuildGeodeRobot bp state =
  case bp.geodeRobotCost of
    (ore, obsidian) -> state.pack.ore >= ore && state.pack.obsidian >= obsidian

canBuildObsidianRobot :: Blueprint -> State -> Bool
canBuildObsidianRobot bp state =
  case bp.obsidianRobotCost of
    (ore, clay) -> state.pack.ore >= ore && state.pack.clay >= clay

canBuildClayRobot :: Blueprint -> State -> Bool
canBuildClayRobot bp state = state.pack.ore >= bp.clayRobotCost

canBuildOreRobot :: Blueprint -> State -> Bool
canBuildOreRobot bp state = state.pack.ore >= bp.oreRobotCost

buildGeodeRobot :: Blueprint -> State -> State
buildGeodeRobot bp state =
  state
    { pack =
        state.pack
          { geodeRobots = state.pack.geodeRobots + 1
          , ore = state.pack.ore - fst bp.geodeRobotCost
          , obsidian = state.pack.obsidian - snd bp.geodeRobotCost
          }
    }

buildObsidianRobot :: Blueprint -> State -> State
buildObsidianRobot bp state =
  state
    { pack =
        state.pack
          { obsidianRobots = state.pack.obsidianRobots + 1
          , ore = state.pack.ore - fst bp.obsidianRobotCost
          , clay = state.pack.clay - snd bp.obsidianRobotCost
          }
    }

buildClayRobot :: Blueprint -> State -> State
buildClayRobot bp state =
  state
    { pack =
        state.pack
          { clayRobots = state.pack.clayRobots + 1
          , ore = state.pack.ore - bp.clayRobotCost
          }
    }

buildOreRobot :: Blueprint -> State -> State
buildOreRobot bp state =
  state
    { pack =
        state.pack
          { oreRobots = state.pack.oreRobots + 1
          , ore = state.pack.ore - bp.oreRobotCost
          }
    }

data State
  = State
  { minute :: Int
  , pack :: Pack
  }
  deriving (Show, Eq, Ord)

tick :: State -> State
tick state =
  State
    { minute = state.minute + 1
    , pack =
        Pack
          { oreRobots = state.pack.oreRobots
          , clayRobots = state.pack.clayRobots
          , obsidianRobots = state.pack.obsidianRobots
          , geodeRobots = state.pack.geodeRobots
          , ore = state.pack.ore + state.pack.oreRobots
          , clay = state.pack.clay + state.pack.clayRobots
          , obsidian = state.pack.obsidian + state.pack.obsidianRobots
          , geode = state.pack.geode + state.pack.geodeRobots
          }
    }

data Pack
  = Pack
  { oreRobots :: Int
  , clayRobots :: Int
  , obsidianRobots :: Int
  , geodeRobots :: Int
  , ore :: Int
  , clay :: Int
  , obsidian :: Int
  , geode :: Int
  }
  deriving (Show, Eq, Ord)

initialState :: State
initialState = State 1 (Pack 1 0 0 0 0 0 0 0)

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

canBeat :: State -> Int -> Bool
canBeat state currentMax =
  let
    remaining = timeBudget - state.minute
    maxFutureGeodes =
      remaining * state.pack.geodeRobots
        + (remaining * (remaining - 1) `div` 2)
   in
    state.pack.geode + maxFutureGeodes <= currentMax

maximumNumberOfGeodes :: Blueprint -> Int
maximumNumberOfGeodes bp = fst' $ go M.empty 0 initialState
  where
    go :: M.Map Pack (Int, Int) -> Int -> State -> (Int, M.Map Pack (Int, Int), Int)
    go cache currentMax state
      | canBeat state currentMax = (0, cache, currentMax)
      | otherwise =
          case M.lookup state.pack cache of
            Just (minute, nGeodes) | state.minute >= minute -> (nGeodes, cache, currentMax)
            _ ->
              if state.minute == timeBudget
                then (state.pack.geode, M.insert state.pack (state.minute, state.pack.geode) cache, max state.pack.geode currentMax)
                else
                  let
                    (result1, cache1, max1) =
                      if canBuildGeodeRobot bp state
                        then go cache currentMax (buildGeodeRobot bp $ tick state)
                        else (0, cache, currentMax)
                    newMax1 = max currentMax max1
                   in
                    let (result2, cache2, max2) =
                          if canBuildObsidianRobot bp state
                            then go cache1 newMax1 (buildObsidianRobot bp $ tick state)
                            else (0, cache1, newMax1)
                        newMax2 = max newMax1 max2
                     in let (result3, cache3, max3) =
                              if canBuildClayRobot bp state
                                then go cache2 newMax2 (buildClayRobot bp $ tick state)
                                else (0, cache2, newMax2)
                            newMax3 = max newMax2 max3
                         in let (result4, cache4, max4) =
                                  if canBuildOreRobot bp state
                                    then go cache3 newMax3 (buildOreRobot bp $ tick state)
                                    else (0, cache3, newMax3)
                                newMax4 = max newMax3 max4
                             in let (result5, cache5, max5) = go cache4 newMax4 (tick state)
                                    result6 = maximum [result1, result2, result3, result4, result5]
                                    cache6 = M.insert state.pack (state.minute, result6) cache5

                                    newMax5 = max newMax4 max5
                                 in (result6, cache6, newMax5)

part1 :: Input -> Int
part1 = sum . map blueprintQuality

part2 :: Input -> Int
part2 = const 0

main :: IO ()
main = do
  [args] <- getArgs
  input <- parseInput args
  print $ part1 input
  print $ part2 input
