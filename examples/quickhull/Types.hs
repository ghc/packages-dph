{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module Types ( Point(..), Line(..), points, xsOf, ysOf) where

import Data.Array.Parallel.Prelude

data Point = Point Double Double
data Line  = Line  Point Point

points' :: [:Double:] -> [:Double:] -> [:Point:]
points' xs ys = zipWithP Point xs ys

points :: PArray Double -> PArray Double -> PArray Point
{-# NOINLINE points #-}
points xs ys = toPArrayP (points' (fromPArrayP xs) (fromPArrayP ys))

xsOf' :: [:Point:] -> [:Double:]
xsOf' ps = [: x | Point x _ <- ps :]

xsOf :: PArray Point -> PArray Double
{-# NOINLINE xsOf #-}
xsOf ps = toPArrayP (xsOf' (fromPArrayP ps))

ysOf' :: [:Point:] -> [:Double:]
ysOf' ps = [: y | Point _ y <- ps :]

ysOf :: PArray Point -> PArray Double
{-# NOINLINE ysOf #-}
ysOf ps = toPArrayP (ysOf' (fromPArrayP ps))


