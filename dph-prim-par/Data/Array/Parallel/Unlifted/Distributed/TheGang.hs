{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

--
module Data.Array.Parallel.Unlifted.Distributed.TheGang (
  theGang
) where
import Data.Array.Parallel.Unlifted.Distributed.Gang 
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc (numCapabilities)


-- | DPH programs use this single, shared gang of threads.
--   The gang exists at top level, and is initialised unsafely.
-- 
--   The vectoriser guarantees that the gang is only used by a single
--   computation at a time. This is true because the program produced
--   by the vector only uses flat parallelism, so parallel computations
--   don't invoke further parallel computations. If the vectorised program
--   tries to use nested parallelism then there is a bug in the vectoriser,
--   and we'll get an exception at runtime.
--
theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO (forkGang numCapabilities)

