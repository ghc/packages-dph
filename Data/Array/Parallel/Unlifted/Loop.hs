-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Loop
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (GADTS)
--
-- Description ---------------------------------------------------------------
--
-- Loop/replicate combinators (reexported)
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Loop (
  module Data.Array.Parallel.Unlifted.Flat.Loop,
  module Data.Array.Parallel.Unlifted.Segmented.Loop
) where

import Data.Array.Parallel.Unlifted.Flat.Loop
import Data.Array.Parallel.Unlifted.Segmented.Loop

