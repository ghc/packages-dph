{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module Vectorised
    (solvePA)
where

import CommonVectorised
import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double        as D
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude    as P


{-# NOINLINE solvePA #-}
solvePA
    :: PArray Vec3           -- ^ vertices of the surface
    -> PArray (Int,Int,Int)  -- ^ triangles, each 3 vertex indices
    -> PArray Vec3           -- ^ rays to cast
    -> Double                -- ^ time, used for rotation
    -> PArray (Vec3,Double)  -- ^ rays and their distance
solvePA vertices triangles rays time
 = toPArrayP (solveV (fromPArrayP vertices) (fromPArrayP triangles) (fromPArrayP rays) time)


-- | Cast all rays into triangle mesh
solveV 
    :: [:Vec3:]             -- ^ vertices of the surface
    -> [:(Int,Int,Int):]    -- ^ triangles, each 3 vertex indices
    -> [:Vec3:]             -- ^ rays to cast
    -> Double               -- ^ time
    -> [:(Vec3,Double):]    -- ^ rays and their distance
solveV vertices triangles rays time
 = mapP cast' rays
 where
  cast' = cast vertices triangles time


-- | Cast a single ray into the rotated triangle mesh
cast 
    :: [:Vec3:]          -- ^ vertices of the surface
    -> [:(Int,Int,Int):] -- ^ triangles, each 3 vertex indices
    -> Double            -- ^ time
    -> Vec3              -- ^ ray
    -> (Vec3,Double)
cast vertices triangles time ray
 = let r' = ((0,0,0), ray)
       pl = plueckerOfLine r'
       mi = minimumP
            (mapP (\t -> check r' pl (tri vertices t time)) triangles)
   in  (ray, mi)

-- | Return scale / distance along line's direction for intersection. Or a large number if there is no intersection.
check :: Line -> Pluecker -> Triangle -> Double
check r pl t
  | inside pl t = lineOnTriangle r t
  | otherwise = 1e100

-- Get all triangle points from mesh, rotated depending on time
tri :: [:Vec3:] -> (Int,Int,Int) -> Double -> Triangle
tri v (a,b,c) time = (get a, get b, get c)
 where
  {-# INLINE get #-}
  get i = rotate (v !: i) (time / 4)
