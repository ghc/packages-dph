{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.Locked
        ( lockedZip,    lockedZipWith
        , lockedZip3,   lockedZipWith3
        , lockedZip4,   lockedZipWith4
        , lockedZip5,   lockedZipWith5
        , lockedZip6,   lockedZipWith6
        , lockedZip7,   lockedZipWith7
        , lockedZip8,   lockedZipWith8)
where
import Data.Array.Parallel.Unlifted.Stream.Locked
import Data.Vector.Generic               as G


-- Locked Zips ----------------------------------------------------------------
-- | Zip two vectors of the same length.
--   If they do not have the same length then the result is undefined.
lockedZip
        :: ( Vector v a, Vector v b
           , Vector v (a, b))
        => v a -> v b
        -> v (a, b)

lockedZip aa bb
        = unstream $ stream2 aa bb 
{-# INLINE_U lockedZip #-}


-- | Zip three vectors of the same length.
lockedZip3
        :: ( Vector v a, Vector v b, Vector v c
           , Vector v (a, b, c))
        => v a -> v b -> v c
        -> v (a, b, c)

lockedZip3 aa bb cc
        = unstream $ stream3 aa bb cc
{-# INLINE_U lockedZip3 #-}


-- | Zip four vectors of the same length.
lockedZip4
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v (a, b, c, d))
        => v a -> v b -> v c -> v d
        -> v (a, b, c, d)

lockedZip4 aa bb cc dd
        = unstream $ stream4 aa bb cc dd
{-# INLINE_U lockedZip4 #-}


-- | Zip five vectors of the same length.
lockedZip5
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e
           , Vector v (a, b, c, d, e))
        => v a -> v b -> v c -> v d -> v e
        -> v (a, b, c, d, e)

lockedZip5 aa bb cc dd ee
        = unstream $ stream5 aa bb cc dd ee
{-# INLINE_U lockedZip5 #-}


-- | Zip six vectors of the same length.
lockedZip6
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f
           , Vector v (a, b, c, d, e, f))
        => v a -> v b -> v c -> v d -> v e -> v f
        -> v (a, b, c, d, e, f)

lockedZip6 aa bb cc dd ee ff
        = unstream $ stream6 aa bb cc dd ee ff
{-# INLINE_U lockedZip6 #-}


-- | Zip seven vectors of the same length.
lockedZip7
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g
           , Vector v (a, b, c, d, e, f, g))
        => v a -> v b -> v c -> v d -> v e -> v f -> v g
        -> v (a, b, c, d, e, f, g)

lockedZip7 aa bb cc dd ee ff gg
        = unstream $ stream7 aa bb cc dd ee ff gg
{-# INLINE_U lockedZip7 #-}


-- | Zip eight vectors of the same length.
lockedZip8
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v (a, b, c, d, e, f, g, h))
        => v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h
        -> v (a, b, c, d, e, f, g, h)

lockedZip8 aa bb cc dd ee ff gg hh
        = unstream $ stream8 aa bb cc dd ee ff gg hh
{-# INLINE_U lockedZip8 #-}


-- Locked ZipWiths ------------------------------------------------------------
lockedZipWith
        :: ( Vector v a, Vector v b, Vector v c
           , Vector v (a, b))
        => (a -> b -> c)
        -> v a -> v b -> v c

lockedZipWith f aa bb
        = G.map (\(a, b) -> f a b)
        $ lockedZip aa bb
{-# INLINE lockedZipWith #-}


lockedZipWith3  
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v (a, b, c))
        => (a -> b -> c -> d)
        -> v a -> v b -> v c -> v d

lockedZipWith3 f aa bb cc
        = G.map (\(a, b, c) -> f a b c)
        $ lockedZip3 aa bb cc
{-# INLINE lockedZipWith3 #-}


lockedZipWith4
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e
           , Vector v (a, b, c, d))
        => (a -> b -> c -> d -> e)
        -> v a -> v b -> v c -> v d -> v e

lockedZipWith4 f aa bb cc dd
        = G.map (\(a, b, c, d) -> f a b c d)
        $ lockedZip4 aa bb cc dd
{-# INLINE lockedZipWith4 #-}


lockedZipWith5
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f
           , Vector v (a, b, c, d, e))
        => (a -> b -> c -> d -> e -> f)
        -> v a -> v b -> v c -> v d -> v e -> v f

lockedZipWith5 f aa bb cc dd ee
        = G.map (\(a, b, c, d, e) -> f a b c d e)
        $ lockedZip5 aa bb cc dd ee
{-# INLINE lockedZipWith5 #-}


lockedZipWith6
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g
           , Vector v (a, b, c, d, e, f))
        => (a -> b -> c -> d -> e -> f -> g)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g

lockedZipWith6 fn aa bb cc dd ee ff
        = G.map (\(a, b, c, d, e, f) -> fn a b c d e f)
        $ lockedZip6 aa bb cc dd ee ff
{-# INLINE lockedZipWith6 #-}


lockedZipWith7
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v (a, b, c, d, e, f, g))
        => (a -> b -> c -> d -> e -> f -> g -> h)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h

lockedZipWith7 fn aa bb cc dd ee ff gg
        = G.map (\(a, b, c, d, e, f, g) -> fn a b c d e f g)
        $ lockedZip7 aa bb cc dd ee ff gg
{-# INLINE lockedZipWith7 #-}


lockedZipWith8
        :: ( Vector v a, Vector v b, Vector v c, Vector v d
           , Vector v e, Vector v f, Vector v g, Vector v h
           , Vector v i
           , Vector v (a, b, c, d, e, f, g, h))
        => (a -> b -> c -> d -> e -> f -> g -> h -> i)
        -> v a -> v b -> v c -> v d -> v e -> v f -> v g -> v h -> v i

lockedZipWith8 fn aa bb cc dd ee ff gg hh
        = G.map (\(a, b, c, d, e, f, g, h) -> fn a b c d e f g h)
        $ lockedZip8 aa bb cc dd ee ff gg hh
{-# INLINE lockedZipWith8 #-}


