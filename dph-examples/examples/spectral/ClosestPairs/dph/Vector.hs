{-# LANGUAGE BangPatterns #-}
module Vector (closestV, closeststupidV) where
import Points2D.Types
import qualified Data.Vector            as V
import qualified Data.Vector.Unboxed    as U

-- removed the sqrt here - only some users actually need it
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2)
  = ( (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) )

-- Distance between two points, but return a very large number if they're the same...
distancex :: Point -> Point -> Double
distancex a b
 = let d = distance a b
   in  if d == 0 then 1e100 else d


-- An n^2 algorithm for finding closest pairs.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: U.Vector Point -> U.Vector (Point,Point)
closeststupid pts
 = U.map m1 pts
 where
  m1 a = (a, U.minimumBy (m2 a) pts)
  m2 a b c = distancex a b `compare` distancex a c


near_boundary :: U.Vector (Point,Point) -> Double -> Double -> U.Vector (Int,(Point,Point))
near_boundary a x0 d
 = U.filter check (U.indexed a)
 where
  check (_,((x1,_),_)) = abs (x1 - x0) < d


new_nearest :: U.Vector (Int,(Point,Point)) -> U.Vector (Int,(Point,Point)) -> U.Vector (Int,(Point,Point))
new_nearest a b
 | U.length b == 0 = a
 | otherwise
 = let bp = U.map (\(_,(pt1,_)) -> pt1) b
   in  U.map (m1 bp) a
 where
  m1 bp (k,(pt1,pt2))
   = let pn = U.minimumBy (\b c -> distancex pt1 b `compare` distancex pt1 c) bp
     in  (k, (pt1, check pt1 pn pt2))
  check pt1 pn pt2
   = if   distancex pt1 pn < distancex pt1 pt2
     then pn
     else pt2

merge_pairs :: Double -> U.Vector (Point,Point) -> U.Vector (Point,Point) -> U.Vector (Point,Point)
merge_pairs x0 a b
 = let d  = sqrt (max (U.maximum (U.map dist a)) (U.maximum (U.map dist b)))
       an = near_boundary a x0 d
       bn = near_boundary b x0 d
       a' = a `U.update` new_nearest an bn
       b' = b `U.update` new_nearest bn an
   in  a' U.++ b'
 where
  dist (a,b) = distancex a b


closest :: U.Vector Point -> U.Vector (Point,Point)
closest pts
 | U.length pts < 250 = closeststupid pts
 | otherwise        =
   let (xs,ys)   = U.unzip pts

       xd   = U.maximum xs - U.minimum xs
       yd   = U.maximum ys - U.minimum ys

       mid  = median xs
       
       top  = U.filter (\(x,_) -> x >= mid) pts
       bot  = U.filter (\(x,_) -> x <  mid) pts

       top' = closest top
       bot' = closest bot

   in  merge_pairs mid top' bot'





closestV :: U.Vector Point -> U.Vector (Point,Point)
closestV = closest

closeststupidV :: U.Vector Point -> U.Vector (Point,Point)
closeststupidV = closeststupid


median :: U.Vector Double -> Double
median xs = median' xs (U.length xs `div` 2)

median':: U.Vector Double -> Int -> Double 
median' xs k =
  let p  = xs U.! (U.length xs `div` 2)
      ls = U.filter (<p) xs
  in  if   k < (U.length ls)
      then median' ls k
      else
        let gs  = U.filter (>p) xs
            len = U.length xs - U.length gs
        in  if   k >= len
            then median' gs (k - len)
            else p


