{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Arrow (Arrow (first, second))
import Data.Char (isNumber)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.List.Split
import Data.Word
import GHC.Generics
import System.Environment

data Blueprint
  = Blueprint
  { blueprintId :: !Word8
  , oreRobotCost :: !Word8
  , clayRobotCost :: !Word8
  , obsidianRobotCost :: !(Word8, Word8)
  , geodeRobotCost :: !(Word8, Word8)
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

timeBudget :: Word8
timeBudget = 24

blueprintQuality :: Blueprint -> Int
blueprintQuality bp =
  let nGeodes = maximumNumberOfGeodes bp
   in -- trace ("BP: " <> show bp.blueprintId <> ": " <> show nGeodes) $
      fromIntegral bp.blueprintId * fromIntegral nGeodes

canBuildGeodeRobot :: Blueprint -> State -> Bool
canBuildGeodeRobot bp state =
  case bp.geodeRobotCost of
    (ore, obsidian) -> state.pack.ore >= fromIntegral ore && state.pack.obsidian >= fromIntegral obsidian

canBuildObsidianRobot :: Blueprint -> State -> Bool
canBuildObsidianRobot bp state =
  case bp.obsidianRobotCost of
    (ore, clay) -> state.pack.ore >= fromIntegral ore && state.pack.clay >= fromIntegral clay

canBuildClayRobot :: Blueprint -> State -> Bool
canBuildClayRobot bp state = state.pack.ore >= fromIntegral bp.clayRobotCost

canBuildOreRobot :: Blueprint -> State -> Bool
canBuildOreRobot bp state = state.pack.ore >= fromIntegral bp.oreRobotCost

buildGeodeRobot :: Blueprint -> State -> State
buildGeodeRobot bp state =
  state
    { pack =
        state.pack
          { geodeRobots = state.pack.geodeRobots + 1
          , ore = state.pack.ore - fromIntegral (fst bp.geodeRobotCost)
          , obsidian = state.pack.obsidian - fromIntegral (snd bp.geodeRobotCost)
          }
    }

buildObsidianRobot :: Blueprint -> State -> State
buildObsidianRobot bp state =
  state
    { pack =
        state.pack
          { obsidianRobots = state.pack.obsidianRobots + 1
          , ore = state.pack.ore - fromIntegral (fst bp.obsidianRobotCost)
          , clay = state.pack.clay - fromIntegral (snd bp.obsidianRobotCost)
          }
    }

buildClayRobot :: Blueprint -> State -> State
buildClayRobot bp state =
  state
    { pack =
        state.pack
          { clayRobots = state.pack.clayRobots + 1
          , ore = state.pack.ore - fromIntegral bp.clayRobotCost
          }
    }

buildOreRobot :: Blueprint -> State -> State
buildOreRobot bp state =
  state
    { pack =
        state.pack
          { oreRobots = state.pack.oreRobots + 1
          , ore = state.pack.ore - fromIntegral bp.oreRobotCost
          }
    }

data State
  = State
  { minute :: !Word8
  , pack :: !Pack
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
          , ore = state.pack.ore + fromIntegral state.pack.oreRobots
          , clay = state.pack.clay + fromIntegral state.pack.clayRobots
          , obsidian = state.pack.obsidian + fromIntegral state.pack.obsidianRobots
          , geode = state.pack.geode + fromIntegral state.pack.geodeRobots
          }
    }

data Pack
  = Pack
  { oreRobots :: !Word8
  , clayRobots :: !Word8
  , obsidianRobots :: !Word8
  , geodeRobots :: !Word8
  , ore :: !Word8
  , clay :: !Word8
  , obsidian :: !Word8
  , geode :: !Word8
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Pack

initialState :: State
initialState = State 0 (Pack 1 0 0 0 0 0 0 0)

canBeat :: State -> StateM (s, Word8) Bool
canBeat state = do
  currentMax <- gets snd
  let
    remaining = timeBudget - state.minute
    maxFutureGeodes =
      remaining * state.pack.geodeRobots
        + remaining `div` 2 * (remaining - 1)

  pure $ state.pack.geode + maxFutureGeodes <= currentMax

maxOreCost :: Blueprint -> Word8
maxOreCost bp = maximum [bp.oreRobotCost, bp.clayRobotCost, fst bp.obsidianRobotCost, fst bp.geodeRobotCost]

maxClayCost :: Blueprint -> Word8
maxClayCost bp = snd bp.obsidianRobotCost

maxObsidianCost :: Blueprint -> Word8
maxObsidianCost bp = snd bp.geodeRobotCost

newtype StateM s a = StateM {runState :: s -> (s, a)}

get :: StateM a a
get = StateM $ \s -> (s, s)

gets :: (a -> b) -> StateM a b
gets f = fmap f get

put :: s -> StateM s ()
put s = StateM $ const (s, ())

modify :: (t -> t) -> StateM t ()
modify f = StateM $ \s -> (f s, ())

execState :: s -> StateM s a -> a
execState s act = snd $ runState act s

instance Functor (StateM s) where
  fmap f = StateM . fmap (second f) . runState

instance Applicative (StateM s) where
  pure x = StateM $ \s -> (s, x)

  StateM f <*> StateM x = StateM $ \s ->
    let (s', f') = f s
     in second f' $ x s'

instance Monad (StateM s) where
  StateM x >>= f = StateM $ \s ->
    let (s', a) = x s
     in runState (f a) s'

maximumNumberOfGeodes :: Blueprint -> Word8
maximumNumberOfGeodes bp = execState (HM.empty, 0) $ go True True True initialState
  where
    go :: Bool -> Bool -> Bool -> State -> StateM (HM.HashMap Pack (Word8, Word8), Word8) Word8
    go canOre canClay canObsidian state = do
      beatable <- canBeat state
      if beatable
        then pure 0
        else do
          cachedResult <- gets (HM.lookup state.pack . fst)
          case cachedResult of
            Just (minute, nGeodes) | state.minute >= minute -> pure nGeodes
            _
              | state.minute == timeBudget -> do
                  modify $ \(cache, currentMax) -> (HM.insert state.pack (state.minute, state.pack.geode) cache, max state.pack.geode currentMax)
                  pure state.pack.geode
              | otherwise -> do
                  result1 <-
                    if canBuildGeodeRobot bp state
                      then go True True True (buildGeodeRobot bp $ tick state)
                      else pure 0

                  (result2, newCanObsidian) <-
                    if canObsidian && state.pack.obsidianRobots < maxObsidianCost bp && canBuildObsidianRobot bp state
                      then (,True) <$> go True True True (buildObsidianRobot bp $ tick state)
                      else
                        pure $
                          if canBuildObsidianRobot bp state
                            then (0, False)
                            else (0, True)
                  (result3, newCanClay) <-
                    if canClay && state.pack.clayRobots < maxClayCost bp && canBuildClayRobot bp state
                      then (,True) <$> go True True True (buildClayRobot bp $ tick state)
                      else
                        pure $
                          if canBuildClayRobot bp state
                            then (0, False)
                            else (0, True)
                  (result4, newCanOre) <-
                    if canOre && state.pack.oreRobots < maxOreCost bp && canBuildOreRobot bp state
                      then (,True) <$> go True True True (buildOreRobot bp $ tick state)
                      else
                        pure $
                          if canBuildOreRobot bp state
                            then (0, False)
                            else (0, True)

                  result5 <- go newCanOre newCanClay newCanObsidian (tick state)
                  let result6 = maximum [result1, result2, result3, result4, result5]
                  modify (first (HM.insert state.pack (state.minute, result6)))

                  pure result6

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
