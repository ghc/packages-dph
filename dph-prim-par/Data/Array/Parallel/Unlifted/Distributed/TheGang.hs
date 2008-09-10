-- This is a hack which will go away as soon as possible!

module Data.Array.Parallel.Unlifted.Distributed.TheGang (
  theGang
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang 

import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc (numCapabilities)

theGang :: Gang
{-# NOINLINE theGang #-}
theGang = unsafePerformIO (forkGang numCapabilities)

