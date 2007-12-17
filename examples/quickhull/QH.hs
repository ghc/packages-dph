{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module QH (quickHull) where

import Types

import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Double
import qualified Data.Array.Parallel.Prelude.Int as Int

import qualified Prelude

distance :: Point -> Line -> Double
distance (Point xo yo) (Line (Point x1 y1) (Point x2 y2))
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)

hsplit points line@(Line p1 p2)
  | lengthP packed Int.< 2 = singletonP p1 +:+ packed
  | otherwise
  = concatP [: hsplit packed ends
               | ends <- singletonP (Line p1 pm) +:+ singletonP (Line pm p2) :]
  where
    cross  = [: distance p line | p <- points :]
    packed = [: p | (p,c) <- zipP points cross, c > 0.0 :]

    pm     = points !: maxIndexP cross

quickHull' points
  = concatP [: hsplit points ends
               | ends <- singletonP (Line minx maxx)
                         +:+ singletonP (Line maxx minx) :]
  where
    xs   = [: x | Point x y <- points :]
    minx = points !: minIndexP xs
    maxx = points !: maxIndexP xs

quickHull :: PArray Point -> PArray Point
quickHull ps = toPArrayP (quickHull' (fromPArrayP ps))

