-- This program is heavily GC bound unless we use a very large heap (eg, -H500M)

import GHC.Conc (par, pseq)

import System.IO
import Control.Exception (evaluate)
import System.Environment
import System.CPUTime
import System.Time
import System.Random

import qualified Data.Array.Parallel.Unlifted as U


-- Time
--


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

loadPoints :: String -> IO [Point]
loadPoints file
  = do
      h <- openBinaryFile file ReadMode
      upts <- U.hGet h
      hClose h
      convert (U.fsts upts) (U.snds upts)

convert :: U.Array Double -> U.Array Double -> IO [Point]
convert xs ys
  = do
      let pts = zipWith Point (U.toList xs) (U.toList ys)
      evaluate $ nf pts
      return pts


-- Benchmark
-- 

data Point = Point !Double !Double
data Line  = Line  Point Point

instance Show Point where
  show (Point x y) = show (x, y)
  
nf (Point x y:xs) = x `seq` y `seq` nf xs
nf []             = ()

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


-- Parallel version

hsplitPar :: [Point] -> Line -> [Point]
hsplitPar points line@(Line p1 p2)
  | length packed < 2 = p1:packed
  | otherwise         = let left  = hsplitPar packed (Line p1 pm)
                            right = hsplitPar packed (Line pm p2)
                        in
                        right `par` 
                        (left ++ right)
  where
    cross  = [ (distance p line, p) | p <- points ]
    packed = [ p | (p, (c, _)) <- zip points cross, c > 0.0 ]

    pm     = upper (>) cross

quickHullPar :: [Point] -> [Point]
quickHullPar [] = []
quickHullPar points
  = let left  = hsplitPar points (Line minx maxx)
        right = hsplitPar points (Line maxx minx)
    in
    right `par`
    (left ++ right)
  where
    xs   = [ (x, p) | p@(Point x y) <- points ]
    minx = upper (<) xs
    maxx = upper (>) xs

-- OBSERVATION: If we use nf on 'right' in 'quickHullPar' and 'hsplitPar' (and maybe even 
--   'nf right `par` nf left `pseq` ...') the parallel GC takes a big hit and makes everything much
--   slower.  (Keep in mind that even in the good case, this program spends 2/3 of its running time
--   in the GC.)

-- main
--

main :: IO ()
main
  = do
      [mode, args1, args2] <- getArgs
      let runs = read args1
      --     n    = read args2
      -- 
      -- let pts  = generatePoints n
      -- eval pts `seq` return ()
      pts <- loadPoints args2
      let {-# NOINLINE oneRun #-}       -- important to execute multiple runs
          oneRun pts = do 
                         t1 <- getTime
                         let res = case mode of 
                                     "seq" -> quickHull pts
                                     "par" -> quickHullPar pts
                                     _     -> error "mode must be 'seq' or 'par'"
                         evaluate $ nf res
                         t2 <- getTime
                         return (length res, fromTime (t2 `minus` t1))
      results <- sequence (replicate runs (oneRun pts))

      let (lens, times) = unzip results
          (walls, cpus) = unzip times
      putStrLn $ "Result length = " ++ show (head lens) ++ ": " ++
                 showWallCPU (minimum walls) (minimum cpus) ++ " " ++
                 showWallCPU (sum walls `div` toInteger runs) 
                             (sum cpus  `div` toInteger runs) ++ " " ++
                 showWallCPU (maximum walls) (maximum cpus)
  where
    showWallCPU wall cpu = show wall ++"/" ++ show cpu