module Vector
    (solveV)
where

import Common
import Data.Vector.Unboxed
import Prelude hiding (map,filter,minimum)


{-# NOINLINE solveV #-}
solveV 
    :: Vector Vec3             -- ^ vertices of the surface
    -> Vector (Int,Int,Int)    -- ^ triangles, each 3 vertex indices
    -> Vector Vec3             -- ^ rays to cast
    -> Double                  -- ^ time
    -> Vector (Vec3,Double)    -- ^ rays and their distance
solveV vertices triangles rays time
 = map cast' rays
 where
  cast' = cast vertices triangles time


cast 
    :: Vector Vec3          -- ^ vertices of the surface
    -> Vector (Int,Int,Int) -- ^ triangles, each 3 vertex indices
    -> Double               -- ^ time
    -> Vec3                 -- ^ ray
    -> (Vec3,Double)
cast vertices triangles time ray
 = let r' = ((0,0,0), ray)
       pl = plueckerOfLine r'
       mi = minimum
          $ filter (>0)
          $ map (\t -> check r' pl $ tri vertices t time) triangles
   in  (ray, mi)

check r pl t
  | inside pl t = lineOnTriangle r t
  | otherwise = 1e100

tri :: Vector Vec3 -> (Int,Int,Int) -> Double -> Triangle
tri v (a,b,c) time = (get a, get b, get c)
 where
  {-# INLINE get #-}
  get i = rotate (v `unsafeIndex` i) (time / 4)

