{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.UPArr
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
--		 (c) 2008         Manuel M T Chakravarty & Gabriele Keller & Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (GADTS)
--
-- Description ---------------------------------------------------------------
--
-- This module defines unlifted arrays generically as a GADT.
--
-- Slicing is implemented by each `BUArr' having the slicing information.  A
-- possible alternative design would be to maintain this information in
-- `UArr', but not in the representations, but at the root.  This may seem
-- attractive at first, but seems to be more disruptive without any real
-- benefits _ this is essentially, because we then need the slicing
-- information at each level; ie, also at the leafs where it is sufficient
-- using the current implementation.
--
-- Todo ----------------------------------------------------------------------
--

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Parallel.UPArr () where

-- standard libraries
import Control.Monad (liftM, liftM2)

-- friends
import Data.Array.Parallel.Base
import Data.Array.Parallel.Unlifted.Flat
import Data.Array.Parallel.Unlifted.Distributed

import Data.Array.Parallel.Arr (
  BUArr, MBUArr, UAE,
  lengthBU, indexBU, sliceBU, hGetBU, hPutBU,
  lengthMBU, newMBU, readMBU, writeMBU, copyMBU, unsafeFreezeMBU)

import System.IO

