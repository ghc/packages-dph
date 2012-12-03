module Common where

-- Type synonyms so they work with Data.Vector.Unboxed etc.

-- A 3-vector *or* points
type Vec3     = (Double, Double, Double)
-- Line or ray between two points in space
-- These are both points, so direction of (a,b) is the vector (b-a)
type Line     = (Vec3, Vec3)

-- Pl\"{u}cker coordinates of a line.
-- To convert line (a,b) into pluecker, (b-a, a X b)
type Pluecker = (Vec3,Vec3)

-- Triangles are cool because they're obviously always planar.
type Triangle = (Vec3, Vec3, Vec3)

-- Normal and distance of normal from (0,0,0)
type Plane = (Vec3,Double)


{-# INLINE dot #-}
dot :: Vec3 -> Vec3 -> Double
dot (u,v,w) (x,y,z) = u*x + v*y + w*z

{-# INLINE cross #-}
cross :: Vec3 -> Vec3 -> Vec3
cross (u,v,w) (x,y,z) = ( v * z - w * y
                        , w * x - u * z
                        , u * y - v * x)

{-# INLINE over1 #-}
over1 :: (Double -> Double) -> Vec3 -> Vec3
over1 f (x,y,z) = (f x, f y, f z)

{-# INLINE over2 #-}
over2 :: (Double -> Double -> Double) -> Vec3 -> Vec3 -> Vec3
over2 f (u,v,w) (x,y,z) = (f u x, f v y, f w z)

{-# INLINE mag #-}
mag a = sqrt (a `dot` a)

{-# INLINE norm #-}
norm a = over1 (/m) a
 where m = mag a

{-# INLINE vsub #-}
vadd = over2 (+)
vsub = over2 (-)

-- Convert a line into pluecker coordinates
{-# INLINE plueckerOfLine #-}
plueckerOfLine :: Line -> Pluecker
plueckerOfLine (p,q) = (q `vsub` p, q `cross` p)

-- Find intersection of a line and plucker, if exists.
-- Otherwise 
{-# INLINE projectPluecker2 #-}
projectPluecker2 :: Pluecker -> Pluecker -> Double
projectPluecker2 (u1,v1) (u2,v2) = (u1 `dot` v2) + (u2 `dot` v1)

-- Check whether pluecker line intersects given triangle
{-# INLINE inside #-}
inside :: Pluecker -> Triangle -> Bool
inside p (a,b,c)
 = let pr = projectPluecker2 p . plueckerOfLine
       ab = pr (a,b) < 0
       bc = pr (b,c) < 0
       ca = pr (c,a) < 0
   in  (ab && bc && ca) || not (ab || bc || ca)


-- Get plane from triangle, eg for projection
{-# INLINE planeOfTriangle #-}
planeOfTriangle :: Triangle -> Plane
planeOfTriangle (a,b,c)
 = let n  = (a `vsub` b) `cross` (c `vsub` b)
       n' = norm n
       d  = negate (n' `dot` b)
   in  (n', d)

-- should this return Maybe Double?
{-# INLINE lineOnPlane #-}
lineOnPlane :: Line -> Plane -> Double
lineOnPlane (p,q) (n,d)
 = let d1 = (n `dot` p) + d
       d2 = (n `dot` q) + d
   in  if   d1 == d2
       then 1e100 -- meh. big. fail.
       else d1 / (d1 - d2)

-- Project line onto triangle's plane
-- Disregard whether intersection is actually inside triangle
-- Intersection point is scale from line
{-# INLINE lineOnTriangle #-}
lineOnTriangle :: Line -> Triangle -> Double
lineOnTriangle p t
 = lineOnPlane p (planeOfTriangle t)
