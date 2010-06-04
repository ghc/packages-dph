
module Timing where
import System.CPUTime
import System.Time

data Time = Time { cpu_time  :: Integer
                 , wall_time :: Integer
                 }

type TimeUnit = Integer -> Integer

picoseconds :: TimeUnit
picoseconds = id

milliseconds :: TimeUnit
milliseconds n = n `div` 1000000000

seconds :: TimeUnit
seconds n = n `div` 1000000000000

cpuTime :: TimeUnit -> Time -> Integer
cpuTime f = f . cpu_time

wallTime :: TimeUnit -> Time -> Integer
wallTime f = f . wall_time

getTime :: IO Time
getTime =
  do
    cpu          <- getCPUTime
    TOD sec pico <- getClockTime
    return $ Time cpu (pico + sec * 1000000000000)

zipT :: (Integer -> Integer -> Integer) -> Time -> Time -> Time
zipT f (Time cpu1 wall1) (Time cpu2 wall2) =
  Time (f cpu1 cpu2) (f wall1 wall2)

minus :: Time -> Time -> Time
minus = zipT (-)

fromTime :: Time -> (Integer, Integer)
fromTime t = (wallTime milliseconds t, cpuTime milliseconds t)

instance Show Time where
  showsPrec n t = showsPrec n (wallTime milliseconds t)
                . showChar '/'
                . showsPrec n (cpuTime milliseconds t)

