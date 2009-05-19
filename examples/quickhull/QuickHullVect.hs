{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module QuickHullVect (quickhull) where

import Types

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int

import qualified Prelude as P

distance :: Point -> Line -> Double
distance (Point xo yo) (Line (Point x1 y1) (Point x2 y2))
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hsplit :: [:Point:] -> Line -> [:Point:]
hsplit points line@(Line p1 p2)
  | lengthP packed Int.< 2 = singletonP p1 +:+ packed
  | otherwise
--  = concatP [: hsplit packed ends
--               | ends <- singletonP (Line p1 pm) +:+ singletonP (Line pm p2) :]
  = concatP (mapP (\ends -> hsplit packed ends)
                  (singletonP (Line p1 pm) +:+ singletonP (Line pm p2)))
  where
--    cross  = [: distance p line | p <- points :]
    cross  = mapP (\p -> distance p line) points
--    packed = [: p | (p,c) <- zipP points cross, c > 0.0 :]
    packed = mapP (\(p, _) -> p) 
                  (filterP (\(p, c) -> c > 0.0) (zipP points cross))

    pm     = points !: maxIndexP cross

quickHull' :: [:Point:] -> [:Point:]
quickHull' points
  | lengthP points Int.== 0 = points
  | otherwise
--  = concatP [: hsplit points ends
--               | ends <- singletonP (Line minx maxx)
--                         +:+ singletonP (Line maxx minx) :]
  = concatP (mapP (\ends -> hsplit points ends)
                  (singletonP (Line minx maxx) +:+ singletonP (Line maxx minx)))
  where
--    xs   = [: x | Point x y <- points :]
    xs   = mapP (\(Point x y) -> x) points
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs

quickhull :: PArray Point -> PArray Point
{-# NOINLINE quickhull #-}
quickhull ps = toPArrayP (quickHull' (fromPArrayP ps))

