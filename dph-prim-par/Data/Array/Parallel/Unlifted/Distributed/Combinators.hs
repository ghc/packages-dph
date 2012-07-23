{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Combinators 
        ( What (..)
        , generateD, generateD_cheap
        , imapD, mapD
        , zipD, unzipD
        , fstD, sndD
        , zipWithD, izipWithD
        , foldD
        , scanD
        , mapAccumLD

           -- * Monadic combinators
        , mapDST_, mapDST, zipWithDST_, zipWithDST)
where
import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Gang
import Data.Array.Parallel.Unlifted.Distributed.Types
import Data.Array.Parallel.Unlifted.Distributed.DistST
import Data.Array.Parallel.Unlifted.Distributed.What
import Debug.Trace

here s = "Data.Array.Parallel.Unlifted.Distributed.Combinators." ++ s


-- | Create a distributed value, given a function to create the instance
--   for each thread.
generateD 
        :: DT a 
        => What         -- ^ What is the worker function doing.
        -> Gang 
        -> (Int -> a) 
        -> Dist a
generateD what g f 
        = traceEvent (show $ CompGenerate False what) 
        $ runDistST g (myIndex >>= return . f)
{-# NOINLINE generateD #-}


-- | Create a distributed value, but do it sequentially.
--  
--   This function is used when we want to operate on a distributed value, but
--   there isn't much data involved. For example, if we want to distribute 
--   a single integer to each thread, then there's no need to fire up the 
--   gang for this.
--   
generateD_cheap 
        :: DT a 
        => What          -- ^ What is the worker function doing.
        -> Gang 
        -> (Int -> a) 
        -> Dist a

generateD_cheap what g f 
        = traceEvent (show $ CompGenerate True what) 
        $ runDistST_seq g (myIndex >>= return . f)
{-# NOINLINE generateD_cheap #-}


-- Mapping --------------------------------------------------------------------
--
-- Fusing maps
-- ~~~~~~~~~~~
--  The staging here is important. 
--  Our rewrite rules only operate on the imapD form, so fusion between the worker
--  functions of consecutive maps takes place before phase [0]. 
--
--  At phase [0] we then inline imapD which introduces the call to imapD' which
--  uses the gang to evaluate its (now fused) worker.
--

-- | Map a function to every instance of a distributed value.
--
--   This applies the function to every thread, but not every value held
--   by the thread. If you want that then use something like:
-- 
--   @mapD theGang (V.map (+ 1)) :: Dist (Vector Int) -> Dist (Vector Int)@
--
mapD    :: (DT a, DT b) 
        => What         -- ^ What is the worker function doing.
        -> Gang 
        -> (a -> b) 
        -> Dist a 
        -> Dist b

mapD wFn gang
        = imapD wFn gang . const
{-# INLINE mapD #-}
--  INLINE because this is just a convenience wrapper for imapD.
--  None of our rewrite rules are particular to mapD.


-- | Map a function across all elements of a distributed value.
--   The worker function also gets the current thread index.
--   As opposed to `imapD'` this version also deepSeqs each element before
--   passing it to the function.
imapD   :: (DT a, DT b) 
        => What         -- ^ What is the worker function doing.
        -> Gang 
        -> (Int -> a -> b) 
        -> Dist a -> Dist b
imapD wFn gang f d 
        = imapD' wFn gang (\i x -> x `deepSeqD` f i x) d
{-# INLINE [0] imapD #-}
--  INLINE [0] because we want to wait until phase [0] before introducing
--  the call to imapD'. Our rewrite rules operate directly on the imapD
--  formp, so once imapD is inlined no more fusion can take place.


-- | Map a function across all elements of a distributed value.
--   The worker function also gets the current thread index.
imapD'  :: (DT a, DT b) 
        => What -> Gang -> (Int -> a -> b) -> Dist a -> Dist b
imapD' what gang f !d 
  = traceEvent (show (CompMap $ what))
  $ runDistST gang 
        (do i <- myIndex
            x <- myD d
            return (f i x))
{-# NOINLINE imapD' #-}
-- NOINLINE 


{-# RULES

"imapD/generateD" 
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD wGen gang g) 
  = generateD (WhatFusedMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/generateD_cheap" 
  forall wMap wGen gang f g
  . imapD wMap gang f (generateD_cheap wGen gang g) 
  = generateD (WhatFusedMapGen wMap wGen) gang (\i -> f i (g i))

"imapD/imapD" 
  forall wMap1 wMap2 gang f g d
  . imapD wMap1 gang f (imapD wMap2 gang g d) 
  = imapD (WhatFusedMapMap wMap1 wMap2) gang (\i x -> f i (g i x)) d

  #-}


-- Zipping --------------------------------------------------------------------
-- | Combine two distributed values with the given function.
zipWithD :: (DT a, DT b, DT c)
        => What                 -- ^ What is the worker function doing.
        -> Gang 
        -> (a -> b -> c) 
        -> Dist a -> Dist b -> Dist c

zipWithD what g f dx dy 
        = mapD what g (uncurry f) (zipD dx dy)
{-# INLINE zipWithD #-}


-- | Combine two distributed values with the given function.
--   The worker function also gets the index of the current thread.
izipWithD :: (DT a, DT b, DT c)
          => What               -- ^ What is the worker function doing.
          -> Gang 
          -> (Int -> a -> b -> c) 
          -> Dist a -> Dist b -> Dist c

izipWithD what g f dx dy 
        = imapD what g (\i -> uncurry (f i)) (zipD dx dy)
{-# INLINE izipWithD #-}


{-# RULES
"zipD/imapD[1]" 
  forall gang f xs ys what
  . zipD (imapD what gang f xs) ys
  = imapD what gang (\i (x,y) -> (f i x, y)) (zipD xs ys)

"zipD/imapD[2]" 
  forall gang f xs ys what
  . zipD xs (imapD what gang f ys)
  = imapD what gang (\i (x,y) -> (x, f i y)) (zipD xs ys)

"zipD/generateD[1]" 
  forall gang f xs what
  . zipD (generateD what gang f) xs
  = imapD what gang (\i x -> (f i, x)) xs

"zipD/generateD[2]" 
  forall gang f xs what
  . zipD xs (generateD what gang f)
  = imapD what gang (\i x -> (x, f i)) xs

  #-}


-- Folding --------------------------------------------------------------------
-- | Fold all the instances of a distributed value.
foldD :: DT a => Gang -> (a -> a -> a) -> Dist a -> a
foldD g f !d 
  = checkGangD ("here foldD") g d 
  $ fold 1 (indexD (here "foldD") d 0)
  where
    !n = gangSize g
    --
    fold i x | i == n    = x
             | otherwise = fold (i+1) (f x $ indexD (here "foldD") d i)
{-# NOINLINE foldD #-}


-- | Prefix sum of the instances of a distributed value.
scanD :: forall a. DT a => Gang -> (a -> a -> a) -> a -> Dist a -> (Dist a, a)
scanD g f z !d
  = checkGangD (here "scanD") g d 
  $ runST (do
          md <- newMD g
          s  <- scan md 0 z
          d' <- unsafeFreezeMD md
          return (d',s))
  where
    !n = gangSize g
    
    scan :: forall s. MDist a s -> Int -> a -> ST s a
    scan md i !x
        | i == n    = return x
        | otherwise
        = do    writeMD md i x
                scan md (i+1) (f x $ indexD (here "scanD") d i)
{-# NOINLINE scanD #-}



-- MapAccumL ------------------------------------------------------------------
-- | Combination of map and fold.
mapAccumLD 
        :: forall a b acc. (DT a, DT b)
        => Gang
        -> (acc -> a      -> (acc, b))
        ->  acc -> Dist a -> (acc, Dist b)

mapAccumLD g f acc !d
  = checkGangD (here "mapAccumLD") g d 
  $ runST (do
        md   <- newMD g
        acc' <- go md 0 acc
        d'   <- unsafeFreezeMD md
        return (acc',d'))
  where
    !n = gangSize g
    
    go :: MDist b s -> Int -> acc -> ST s acc
    go md i acc'
        | i == n    = return acc'
        | otherwise
        = case f acc' (indexD (here "mapAccumLD") d i) of
                (acc'',b) -> do
                      writeMD md i b
                      go md (i+1) acc''
{-# INLINE_DIST mapAccumLD #-}
                                

-- Versions that work on DistST -----------------------------------------------
-- NOTE: The following combinators must be strict in the Dists because if they
-- are not, the Dist might be evaluated (in parallel) when it is requested in
-- the current computation which, again, is parallel. This would break our
-- model andlead to a deadlock. Hence the bangs.

mapDST_ :: DT a => Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_ g p d 
 = mapDST_' g (\x -> x `deepSeqD` p x) d
{-# INLINE mapDST_ #-}


mapDST_' :: DT a => Gang -> (a -> DistST s ()) -> Dist a -> ST s ()
mapDST_' g p !d 
 = checkGangD (here "mapDST_") g d 
 $ distST_ g (myD d >>= p)


mapDST :: (DT a, DT b) => Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST g p !d = mapDST' g (\x -> x `deepSeqD` p x) d
{-# INLINE mapDST #-}


mapDST' :: (DT a, DT b) => Gang -> (a -> DistST s b) -> Dist a -> ST s (Dist b)
mapDST' g p !d 
 = checkGangD (here "mapDST_") g d 
 $ distST g (myD d >>= p)


zipWithDST_ 
        :: (DT a, DT b)
        => Gang -> (a -> b -> DistST s ()) -> Dist a -> Dist b -> ST s ()
zipWithDST_ g p !dx !dy 
 = mapDST_ g (uncurry p) (zipD dx dy)
{-# INLINE zipWithDST_ #-}


zipWithDST 
        :: (DT a, DT b, DT c)
        => Gang
        -> (a -> b -> DistST s c) -> Dist a -> Dist b -> ST s (Dist c)
zipWithDST g p !dx !dy 
 = mapDST g (uncurry p) (zipD dx dy)
{-# INLINE zipWithDST #-}

