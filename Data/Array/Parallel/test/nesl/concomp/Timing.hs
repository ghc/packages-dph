module Timing
where

import System.CPUTime
import System.Time

data Time = Time { cpuTime   :: Integer
                 , clockTime :: Integer
                 }

data Timing = Timing { bestTiming    :: Time
                     , averageTiming :: Time
                     , worstTiming   :: Time
                     }

picoseconds :: ClockTime -> Integer
picoseconds (TOD sec pico) = pico + sec * 1000000000000

picoToMilli :: Integer -> Integer
picoToMilli n = n `div` 1000000000

getTime :: IO Time
getTime =
  do
    cpu   <- getCPUTime
    clock <- getClockTime
    return $ Time cpu (picoseconds clock)

zipTime :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipTime f (Time cpu1 clock1) (Time cpu2 clock2) =
  Time (f cpu1 cpu2) (f clock1 clock2)

minusTime :: Time -> Time -> Time
minusTime = zipTime (-)

plusTime :: Time -> Time -> Time
plusTime = zipTime (+)

divTime :: Time -> Integer -> Time
divTime (Time cpu clock) n = Time (cpu `div` n) (clock `div` n)

minTime :: Time -> Time -> Time
minTime = zipTime min

maxTime :: Time -> Time -> Time
maxTime = zipTime max

avgTime :: Time -> Time -> Time
avgTime t1 t2 = (t1 `plusTime` t2) `divTime` 2

sumTime :: [Time] -> Time
sumTime = foldr1 plusTime

minimumTime :: [Time] -> Time
minimumTime = foldr1 minTime

maximumTime :: [Time] -> Time
maximumTime = foldr1 maxTime

averageTime :: [Time] -> Time
averageTime ts = sumTime ts `divTime` toInteger (length ts)

timing :: [Time] -> Timing
timing ts = Timing (minimumTime ts) (averageTime ts) (maximumTime ts)

