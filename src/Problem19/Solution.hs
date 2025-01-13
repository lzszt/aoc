{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Problem19.Solution where

import Data.Char (isDigit)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.List.Split
import Data.Maybe
import Data.Word
import Debug.Trace
import GHC.Generics
import System.IO

data BluePrint = BluePrint
  { bluePrintId :: Int
  , oreRobot :: Word8
  , clayRobot :: Word8
  , obsidianRobot :: (Word8, Word8)
  , geodeRobot :: (Word8, Word8)
  }
  deriving (Show)

data State = State
  { minute :: Int
  , oreRobot :: Word8
  , clayRobot :: Word8
  , obsidianRobot :: Word8
  , geodeRobot :: Word8
  , ore :: Word8
  , clay :: Word8
  , obsidian :: Word8
  , geode :: Word8
  }
  deriving (Show, Generic, Eq)

instance Hashable State

startingState :: State
startingState = State 0 1 0 0 0 0 0 0 0

canBuildGeodeRobot :: BluePrint -> State -> Bool
canBuildGeodeRobot bp state =
  let (neededOre, neededObsidian) = bp.geodeRobot
   in state.ore >= neededOre && state.obsidian >= neededObsidian

canBuildObsidianRobot :: BluePrint -> State -> Bool
canBuildObsidianRobot bp state =
  let (neededOre, neededClay) = bp.obsidianRobot
   in state.ore >= neededOre && state.clay >= neededClay

canBuildClayRobot :: BluePrint -> State -> Bool
canBuildClayRobot bp state = state.ore >= bp.clayRobot

canBuildOreRobot :: BluePrint -> State -> Bool
canBuildOreRobot bp state = state.ore >= bp.oreRobot

buildGeodeRobot :: BluePrint -> State -> Maybe State
buildGeodeRobot bp state
  | canBuildGeodeRobot bp state =
      let (neededOre, neededObsidian) = bp.geodeRobot
       in Just
            state
              { ore = state.ore - neededOre
              , obsidian = state.obsidian - neededObsidian
              , geodeRobot = state.geodeRobot + 1
              }
  | otherwise = Nothing

buildObsidianRobot :: BluePrint -> State -> Maybe State
buildObsidianRobot bp state
  | canBuildObsidianRobot bp state =
      let (neededOre, neededClay) = bp.obsidianRobot
       in Just
            state
              { ore = state.ore - neededOre
              , clay = state.clay - neededClay
              , obsidianRobot = state.obsidianRobot + 1
              }
  | otherwise = Nothing

buildClayRobot :: BluePrint -> State -> Maybe State
buildClayRobot bp state
  | canBuildClayRobot bp state =
      Just
        state
          { ore = state.ore - bp.clayRobot
          , clayRobot = state.clayRobot + 1
          }
  | otherwise = Nothing

buildOreRobot :: BluePrint -> State -> Maybe State
buildOreRobot bp state
  | canBuildOreRobot bp state =
      Just
        state
          { ore = state.ore - bp.oreRobot
          , oreRobot = state.oreRobot + 1
          }
  | otherwise = Nothing

cantBeat :: State -> Int -> Word8 -> Bool
cantBeat state limit maxGeodes =
  let remaining = limit - state.minute
      maxFutureGeodes = remaining * fromIntegral state.geodeRobot + remaining * (remaining - 1) `div` 2
   in fromIntegral state.geode + maxFutureGeodes <= fromIntegral maxGeodes

nextStep :: State -> State
nextStep state =
  state
    { ore = state.ore + state.oreRobot
    , clay = state.clay + state.clayRobot
    , obsidian = state.obsidian + state.obsidianRobot
    , geode = state.geode + state.geodeRobot
    , minute = state.minute + 1
    }

data Stats = Stats
  { cacheRef :: Int
  , cacheHit :: Int
  , statesVisited :: Int
  }
  deriving (Show)

instance Semigroup Stats where
  s1 <> s2 =
    Stats
      (s1.cacheRef + s2.cacheRef)
      (s2.cacheHit + s2.cacheHit)
      (s1.statesVisited + s2.statesVisited)

instance Monoid Stats where
  mempty = Stats 0 0 0

newtype WithCache a = WithCache {unWithCache :: HM.HashMap State Word8 -> Stats -> Word8 -> (HM.HashMap State Word8, Stats, Word8, a)}

instance Functor WithCache where
  fmap f = WithCache . (\g cache stats maxGeodes -> f <$> g cache stats maxGeodes) . unWithCache

instance Applicative WithCache where
  pure x = WithCache $ \cache stats maxGeodes -> (cache, stats, maxGeodes, x)

  WithCache fn <*> WithCache x =
    WithCache $ \cache stats maxGeodes ->
      let (cache', stats', maxGeodes', a) = x cache stats maxGeodes
          (cache'', stats'', maxGeodes'', f) = fn cache' stats' maxGeodes'
       in (cache'', stats'', maxGeodes'', f a)

instance Monad WithCache where
  WithCache a >>= f =
    WithCache $ \cache stats maxGeodes ->
      let (cache', stats', maxGeodes', x) = a cache stats maxGeodes
       in unWithCache (f x) cache' stats' maxGeodes'

modifyCache :: (HM.HashMap State Word8 -> HM.HashMap State Word8) -> WithCache ()
modifyCache f = WithCache $ \cache stats maxGeodes -> (f cache, stats, maxGeodes, ())

gets :: (HM.HashMap State Word8 -> a) -> WithCache a
gets f = WithCache $ \cache stats maxGeodes -> (cache, stats, maxGeodes, f cache)

modifyStats :: (Stats -> Stats) -> WithCache ()
modifyStats f = WithCache $ \cache stats maxGeodes -> (cache, f stats, maxGeodes, ())

modifyMaxGeodes :: (Word8 -> Word8) -> WithCache ()
modifyMaxGeodes f = WithCache $ \cache stats maxGeodes -> (cache, stats, f maxGeodes, ())

getMaxGeodes :: WithCache Word8
getMaxGeodes = WithCache (\cache stats maxGeodes -> (cache, stats, maxGeodes, maxGeodes))

solve :: BluePrint -> Int -> State -> (Stats, Word8)
solve bp limit s = (\(_, y, _, z) -> (y, z)) $ unWithCache (go limit s) HM.empty mempty 0
  where
    go :: Int -> State -> WithCache Word8
    go 0 state = do
      modifyCache (HM.insert state state.geode)
      modifyMaxGeodes (max state.geode)
      pure state.geode
    go n state = do
      maxGeodes <- getMaxGeodes
      if cantBeat state limit maxGeodes
        then pure 0
        else do
          cachedResult <- gets (HM.!? state)
          case cachedResult of
            Just nrGeodes -> do
              modifyStats
                ( \stats ->
                    stats
                      { cacheRef = stats.cacheRef + 1
                      , cacheHit = stats.cacheHit + 1
                      , statesVisited = stats.statesVisited + 1
                      }
                )
              pure nrGeodes
            Nothing -> do
              let actions =
                    [ buildGeodeRobot bp . nextStep
                    , buildObsidianRobot bp . nextStep
                    , buildClayRobot bp . nextStep
                    , buildOreRobot bp . nextStep
                    , Just . nextStep
                    ]
              modifyStats
                ( \stats ->
                    stats
                      { cacheRef = stats.cacheRef + 1
                      , statesVisited = stats.statesVisited + 1
                      }
                )
              nrGeodes <- maximum <$> mapM (go (n - 1)) (mapMaybe ($ state) actions)
              modifyCache (HM.insert state nrGeodes)
              pure nrGeodes

trimap :: (a -> d) -> (b -> e) -> (c -> f) -> (a, b, c) -> (d, e, f)
trimap f g h (x, y, z) = (f x, g y, h z)

loadInput :: FilePath -> IO [BluePrint]
loadInput path = parseInput <$> readFile' path

parseInput :: String -> [BluePrint]
parseInput = map parseBlueprint . lines

parseBlueprint :: String -> BluePrint
parseBlueprint = parse . map (filter isDigit) . filter (any isDigit) . splitOn " "
  where
    parse :: [String] -> BluePrint
    parse [blueprintId, oreForOre, oreForClay, oreForObsidian, clayForObsidian, oreForGeode, obsidianForGeode] =
      BluePrint
        { bluePrintId = read blueprintId
        , oreRobot = read oreForOre
        , clayRobot = read oreForClay
        , obsidianRobot = (read oreForObsidian, read clayForObsidian)
        , geodeRobot = (read oreForGeode, read obsidianForGeode)
        }
    parse inp = error $ "unexpected input: " <> show inp

qualityLevel :: BluePrint -> Int
qualityLevel bp =
  let (stats, nrGeodes) = solve bp 24 startingState
   in traceShow stats bp.bluePrintId * fromIntegral nrGeodes

-- HashMap State Int for limit 19:
-- Stats {cacheRef = 1484704, cacheHit = 572199, statesVisited = 1484704}
-- Stats {cacheRef = 12800643, cacheHit = 5666570, statesVisited = 12800643}
-- 19

-- ________________________________________________________
-- Executed in   74.68 secs    fish           external
--    usr time   70.04 secs  293.00 micros   70.04 secs
--    sys time    4.33 secs  209.00 micros    4.33 secs

-- Replace Int with Word8
-- Stats {cacheRef = 1484704, cacheHit = 572199, statesVisited = 1484704}
-- Stats {cacheRef = 12800643, cacheHit = 5666570, statesVisited = 12800643}
-- 19

-- ________________________________________________________
-- Executed in   77.63 secs    fish           external
--    usr time   73.53 secs    0.00 micros   73.53 secs
--    sys time    3.75 secs  366.00 micros    3.75 secs

-- check if current best can even be beaten
-- Stats {cacheRef = 196522, cacheHit = 62552, statesVisited = 196522}
-- Stats {cacheRef = 999445, cacheHit = 357238, statesVisited = 999445}
-- 19

-- ________________________________________________________
-- Executed in    1.54 secs    fish           external
--    usr time    1.36 secs    0.00 micros    1.36 secs
--    sys time    0.16 secs  362.00 micros    0.16 secs

solution :: IO ()
solution = do
  bp <- head <$> loadInput "./src/Problem19/part1_input_test.txt"
  print $ solve bp 24 startingState

-- blueprints <- loadInput "./src/Problem19/part1_input_test.txt"
-- print $ sum $ map qualityLevel blueprints
