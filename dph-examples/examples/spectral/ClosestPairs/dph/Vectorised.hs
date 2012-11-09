{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}
{-# OPTIONS -fno-spec-constr-count #-}
module Vectorised (closestPA, closeststupidPA) where
import Points2D.Types
import Data.Array.Parallel
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude as P

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2)
  =   (x2 D.- x1) D.* (x2 D.- x1)
  D.+ (y2 D.- y1) D.* (y2 D.- y1)

-- Distance between two points, but return a very large number if they're the same...
distancex :: Point -> Point -> Double
distancex a b
 = let d = distance a b
   in  if d D.== 0 then 1e100 else d


bpermuteP as is = mapP (\i -> as !: i) is

-- An n^2 algorithm for finding closest pairs.
-- Our divide and conquer drops back to this once there are few enough points
closeststupid :: [: Point :] -> [:(Point,Point):]
closeststupid pts
 = let is = [: minIndexP [: distancex a b | b <- pts :] | a <- pts :]
   in  bpermuteP [: (a,b) | a <- pts, b <- pts :] is

near_boundary :: [:(Point,Point):] -> Double -> Double -> [:(Int,(Point,Point)):]
near_boundary a x0 d
 = filterP check (indexedP a)
 where
  check (_,((x1,_),_)) = abs (x1 - x0) < d


new_nearest :: [:(Int,(Point,Point)):] -> [:(Int,(Point,Point)):] -> [:(Int,(Point,Point)):]
new_nearest a b
 | lengthP b I.== 0 = a
 | otherwise
 = let bp = mapP (\(_,(pt1,_)) -> pt1) b
       is = [: minIndexP [: distance pt1 pt2 | pt2 <- bp :] | (_,(pt1,_)) <- a :]
       na = [: (k,(pt1,check pt1 pn pt2)) | (k,(pt1,pt2)) <- a, pn <- bpermuteP bp is :]
   in  na
 where
  check pt1 pn pt2
   = if   distance pt1 pn < distance pt1 pt2
     then pn
     else pt2

merge_pairs :: Double -> [:(Point,Point):] -> [:(Point,Point):] -> [:(Point,Point):]
merge_pairs x0 a b
 = let d  = sqrt (max (maximumP (mapP dist a)) (maximumP (mapP dist b)))
       an = near_boundary a x0 d
       bn = near_boundary b x0 d
       a' = a `updateP` new_nearest an bn
       b' = b `updateP` new_nearest bn an
   in  a' +:+ b'
 where
  dist (a,b) = distance a b


closest :: [:Point:] -> [:(Point,Point):]
closest pts
 | lengthP pts I.< 250 = closeststupid pts
 | otherwise       =
   let (xs,ys)   = unzipP pts

       xd   = maximumP xs D.- minimumP xs
       yd   = maximumP ys D.- minimumP ys

       pts' = if yd D.> xd then flip pts else pts

       mid  = median xs
       
       top  = filterP (\(x,_) -> x D.>= mid) pts
       bot  = filterP (\(x,_) -> x D.<  mid) pts

       top' = closest top
       bot' = closest bot

       pair = merge_pairs mid top' bot'
   in  if yd D.> xd then flip2 pair else pair

flip pts
 = let (xs,ys) = unzipP pts
   in  zipP xs ys

flip2 prs
 = let (p1,p2) = unzipP prs
       (x1,y1) = unzipP p1
       (x2,y2) = unzipP p2
   in  (zipP y1 x1) `zipP` (zipP y2 x2)


closestPA :: PArray Point -> PArray (Point,Point)
closestPA ps = toPArrayP (closest (fromPArrayP ps))

closeststupidPA :: PArray Point -> PArray (Point,Point)
closeststupidPA ps = toPArrayP (closeststupid (fromPArrayP ps))


median :: [: Double :] -> Double
median xs = median' xs (lengthP xs `I.div` 2)

median':: [: Double :] -> Int -> Double 
median' xs k =
  let p  = xs !: (lengthP xs `I.div` 2)
      ls = [:x | x <- xs, x D.< p:]
  in  if   k I.< (lengthP ls)
      then median' ls k
      else
        let gs  = [:x | x <- xs, x D.> p:]
            len = lengthP xs I.- lengthP gs
        in  if   k I.>= len
            then median' gs (k I.- len)
            else p

