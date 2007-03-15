module Timing
where

import System.CPUTime
import System.Time

data Time = Time { cpuTime   :: Integer
                 , clockTime :: Integer
                 }

picoseconds :: Integer -> Integer
picoseconds = id

milliseconds :: Integer -> Integer
milliseconds n = n `div` 1000000000

seconds :: Integer -> Integer
seconds n = n `div` 1000000000000

getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)

timeIO :: IO a -> IO (a, Time)
timeIO p = do
             start <- getTime
             x <- p
             end <- getTime
             return (x, end `minusT` start)

timeIO_ :: IO () -> IO Time
timeIO_ = fmap snd . timeIO

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 clock1) (Time cpu2 clock2) =
  Time (f cpu1 cpu2) (f clock1 clock2)

minusT :: Time -> Time -> Time
minusT = zipT (-)

plusT :: Time -> Time -> Time
plusT = zipT (+)

divT :: Time -> Integer -> Time
divT (Time cpu clock) n = Time (cpu `div` n) (clock `div` n)

minT :: Time -> Time -> Time
minT = zipT min

maxT :: Time -> Time -> Time
maxT = zipT max

avgT :: Time -> Time -> Time
avgT t1 t2 = (t1 `plusT` t2) `divT` 2

sumT :: [Time] -> Time
sumT = foldr1 plusT

minimumT :: [Time] -> Time
minimumT = foldr1 minT

maximumT :: [Time] -> Time
maximumT = foldr1 maxT

averageT :: [Time] -> Time
averageT ts = sumT ts `divT` toInteger (length ts)

