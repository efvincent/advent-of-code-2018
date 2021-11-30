{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--

module AOC.Challenge.Day04 {-- (
  --  day04a
  -- , day04b
  parseLine
  , sample
  , incGuard
  ) -} 
  where

import AOC.Prelude ( (:~>)(..), diffDays, fromGregorian, DiffTime )
import Data.List (sort)
import Data.List.Split (splitOn, splitOneOf)
import Data.IntMap (IntMap, empty, member, insert, (!))
import Data.IntMap as M (lookup)
import Lens.Micro ( (&), (%~), (^.), (?~), (.~)) 
import Lens.Micro.TH (makeLenses)

sample :: String
sample = "[1518-11-01 00:00] Guard #10 begins shift\n\
\[1518-11-01 00:05] falls asleep\n\
\[1518-11-01 00:25] wakes up\n\
\[1518-11-01 00:30] falls asleep\n\
\[1518-11-01 00:55] wakes up\n\
\[1518-11-01 23:58] Guard #99 begins shift\n\
\[1518-11-02 00:40] falls asleep\n\
\[1518-11-02 00:50] wakes up\n\
\[1518-11-03 00:05] Guard #10 begins shift\n\
\[1518-11-03 00:24] falls asleep\n\
\[1518-11-03 00:29] wakes up\n\
\[1518-11-04 00:02] Guard #99 begins shift\n\
\[1518-11-04 00:36] falls asleep\n\
\[1518-11-04 00:46] wakes up\n\
\[1518-11-05 00:03] Guard #99 begins shift\n\
\[1518-11-05 00:45] falls asleep\n\
\[1518-11-05 00:55] wakes up"

yr :: Integer
yr = 1518

data Activity = BeginShift Int | FallsAsleep | WakesUp deriving stock Show

data Time = Time { _hour  :: Int , _minute :: Int  } 
instance Show Time where
  show (Time h m) = show h ++ ":" ++ show m
makeLenses ''Time

data Date = Date { _month :: Int , _day    :: Int  } 
instance Show Date where
  show (Date m d) = show m ++ "/" ++ show d
makeLenses ''Date

data DateTime = DateTime { _date  :: Date, _time   :: Time } 
instance Show DateTime where
  show (DateTime d t) = show d ++ "@" ++ show t
makeLenses ''DateTime

newtype Range = Range (DateTime, DateTime)

instance Show Range where
  show (Range (dt1, dt2)) = show dt1 ++ "-" ++ show dt2

data Guard = Guard { _gid :: Int, _ranges :: [Range] } deriving stock Show
makeLenses ''Guard

type GuardMap = IntMap Guard 

data ParseState = ParseState {
  _sleepStart :: Maybe (Int, DateTime) -- guard ID, when he fell asleep
, _curGuard :: Int
, _guards :: GuardMap
} deriving stock Show
makeLenses ''ParseState

slice :: Int -> Int -> [a] -> [a]
slice a b xs = take (b - a + 1) (drop a xs)

-- | calculates range length in minutes
rangeLen :: Range -> Integer
rangeLen (Range (dt1, dt2)) =
  (ddif * 24 * 60) + (hdif * 60) + mdif - 1
  where
    ddif = diffDays 
              (fromGregorian yr (dt1 ^. (date . month)) (dt1 ^. (date . day))) 
              (fromGregorian yr (dt2 ^. (date . month)) (dt1 ^. (date . day))) 
    hdif = toInteger $ dt2 ^. (time . hour)   - dt1 ^. (time . hour) 
    mdif = toInteger $ dt2 ^. (time . minute) - dt1 ^. (time . minute) 
 
parseLine :: String -> (DateTime, Activity)
parseLine l =
  (DateTime { _date = dt, _time = tm }, act)
  where
    slc f t = slice f t l
    dt = Date { _month = read $ slc 6  7 , _day    = read $ slc 9  10 }
    tm = Time { _hour  = read $ slc 12 13, _minute = read $ slc 15 16 }
    act = 
      case drop 19 l of
        "falls asleep" -> FallsAsleep
        "wakes up"     -> WakesUp
        raw            -> BeginShift . read . drop 1 . (!!1) . splitOn " " $ raw

parse :: String -> GuardMap
parse s =
  go (ParseState Nothing 0 empty) activities 
  where    
    activities = map parseLine . sort . lines $ s
    go pstate [] = pstate ^. guards
    go pstate ((dt,activity):inputLines) =
      case activity of 
        FallsAsleep -> 
        -- when we see "fall asleep", we should already have a current guard, and we
        -- start a sleep range that will be the active sleep range for the current guard
          -- read as "set sleepStart to Just (value of curGuard in pstate) in pstate"
          let pstate' = sleepStart ?~ (pstate ^. curGuard, dt) $ pstate in
          go pstate' inputLines

        WakesUp ->
        -- when we see "wakes up", we should already have a sleeping guard, end the
        -- sleep range, and add it to the guard in the dictionary (adding the guard if not
        -- already in the dictionary)
          let guardMap = pstate ^. guards in
          let (curGuardId,sleepStarted) =
                case pstate ^. sleepStart of
                  Just(cid, ss) -> (cid, ss)
                  Nothing -> error "no current guard when we saw a WakesUp event" in
          let newRange = Range (sleepStarted, dt) in
          let g = case M.lookup curGuardId guardMap of
                    Just g' -> Guard curGuardId (newRange : (g' ^. ranges))
                    Nothing -> Guard curGuardId [] in
          go (sleepStart .~ Nothing $
              (guards .~ insert curGuardId g guardMap $ pstate))
             inputLines
        
        BeginShift curGuardId -> 
          -- get the guard map. If the guard beginning his shift is not in the map, add
          -- him to the map, assuring that on wake and sleep, the guard will be in the dictionary         
          let guardMap =
                let curGuardMap = (pstate ^. guards) in
                case M.lookup curGuardId curGuardMap of 
                  Just _ -> curGuardMap
                  Nothing -> insert curGuardId (Guard curGuardId []) curGuardMap 
                in
          go (curGuard .~ curGuardId $ (guards .~ guardMap $ pstate)) inputLines                  
        
day04a :: _ :~> _
day04a = MkSol
    { sParse = Just 
    , sShow  = show
    , sSolve = Just 
    }

day04b :: _ :~> _
day04b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
