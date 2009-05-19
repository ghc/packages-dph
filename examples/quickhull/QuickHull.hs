import System.Environment
import System.CPUTime
import System.Time
import System.Random


-- Time
--

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


-- Random points generation
--

-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.

generatePoints :: Int -> [Point]
generatePoints n
  = let rg = mkStdGen 42742     -- always use the same seed
    in toPoints (take (2*n) (randomRs (-100, 100) rg))
  where
    toPoints []        = []
    toPoints (x:y:pts) = Point x y : toPoints pts


-- Benchmark
-- 

data Point = Point Double Double
data Line  = Line  Point Point

instance Show Point where
  show (Point x y) = show (x, y)

upper :: (a -> a -> Bool) -> [(a, b)] -> b
upper above = snd . foldl1 pick
  where
    pick left@(kl, _) right@(kr, _) | kl `above` kr = left
                                    | otherwise     = right

distance :: Point -> Line -> Double
distance (Point xo yo) (Line (Point x1 y1) (Point x2 y2))
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hsplit :: [Point] -> Line -> [Point]
hsplit points line@(Line p1 p2)
  | length packed < 2 = p1:packed
  | otherwise         = hsplit packed (Line p1 pm) ++ hsplit packed (Line pm p2)
  where
    cross  = [ (distance p line, p) | p <- points ]
    packed = [ p | (p, (c, _)) <- zip points cross, c > 0.0 ]

    pm     = upper (>) cross

quickHull :: [Point] -> [Point]
quickHull [] = []
quickHull points
  = hsplit points (Line minx maxx) ++ hsplit points (Line maxx minx)
  where
    xs   = [ (x, p) | p@(Point x y) <- points ]
    minx = upper (<) xs
    maxx = upper (>) xs

{-
main = print $ quickHull [Point x y | (x, y) <- pts]
  where
    pts = [(3,3),(2,7),(0,0),(8,5), (4,6),(5,3),(9,6),(10,0)]
 -}

main :: IO ()
main
  = do
      [args1, args2] <- getArgs
      let runs = read args1
          n    = read args2

      let pts  = generatePoints n
      eval pts `seq` return ()
      let {-# NOINLINE oneRun #-}       -- important to execute multiple runs
          oneRun pts = do 
                         t1 <- getTime
                         let len = length $ quickHull pts
                         len `seq` return ()
                         t2 <- getTime
                         return (len, fromTime (t2 `minus` t1))
      results <- sequence (replicate runs (oneRun pts))

      let (lens, times) = unzip results
          (walls, cpus) = unzip times
      putStrLn $ "Result length = " ++ show (head lens) ++ ": " ++
                 showWallCPU (minimum walls) (minimum cpus) ++ " " ++
                 showWallCPU (sum walls `div` toInteger runs) 
                             (sum cpus  `div` toInteger runs) ++ " " ++
                 showWallCPU (maximum walls) (maximum cpus)
  where
    eval (Point x y:xs) = x `seq` y `seq` eval xs
    eval []     = ()

    showWallCPU wall cpu = show wall ++"/" ++ show cpu