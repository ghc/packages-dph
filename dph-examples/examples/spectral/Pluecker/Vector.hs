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
    -> Vector (Vec3,Double)    -- ^ rays and their distance
solveV vertices triangles rays
 = map cast' rays
 where
  cast' = cast vertices triangles


cast 
    :: Vector Vec3          -- ^ vertices of the surface
    -> Vector (Int,Int,Int) -- ^ triangles, each 3 vertex indices
    -> Vec3                 -- ^ ray
    -> (Vec3,Double)
cast vertices triangles ray
 = let r' = ((0,0,0), ray)
       pl = plueckerOfLine r'
       mi = minimum
          $ map (\t -> check r' pl $ tri vertices t) triangles
   in  (ray, mi)

check r pl t
  | inside pl t = lineOnTriangle r t
  | otherwise = 1e100

tri :: Vector Vec3 -> (Int,Int,Int) -> Triangle
tri v (a,b,c) = (v ! a, v ! b, v ! c)

