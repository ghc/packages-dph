
-- Primitive Gang Operators are fundamental computations that run on the gang.
--   At runtime we can record how long each one runs using GHC events.
--   As Gang Operators are not inlined, fusion between them is done via GHC rewrite rules.

-- TODO: rename these to generateG etc, to highlight the fusion level.
{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Standard combinators for distributed types.
module Data.Array.Parallel.Unlifted.Distributed.Primitive.Operators 
        ( generateD
        , generateD_cheap
        , imapD'
        , foldD
        , scanD)
where
import Data.Array.Parallel.Base ( ST, runST)
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DistST
import Data.Array.Parallel.Unlifted.Distributed.Primitive.DT
import Data.Array.Parallel.Unlifted.Distributed.Primitive.Gang
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

