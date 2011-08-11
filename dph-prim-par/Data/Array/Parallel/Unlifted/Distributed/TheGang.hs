
-- DPH programs always used a single, shared gang of threads.
-- The gang exists at top level, and is initialised unsafely.
-- 
-- The Vectoriser guarantees that the gang is only used by a single
-- computation at a time.
--
module Data.Array.Parallel.Unlifted.Distributed.TheGang (
  theGang
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang 

import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc (numCapabilities)

theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO (forkGang numCapabilities)

