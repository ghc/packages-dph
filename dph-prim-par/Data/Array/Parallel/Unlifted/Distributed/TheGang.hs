{-# OPTIONS -Wall -fno-warn-orphans -fno-warn-missing-signatures #-}

-- DPH programs use a single, shared gang of threads.
-- The gang exists at top level, and is initialised unsafely.
-- 
-- The vectoriser guarantees that the gang is only used by a single
-- computation at a time. This is true because the program produced
-- by the vector only uses flat parallelism, so parallel computations
-- don't invoke further parallel computations.
--
-- If the vectorised program tries to use nested parallelism then
--  1) There is a bug in the vectoriser.
--  2) We'll get an exception at runtime.
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

