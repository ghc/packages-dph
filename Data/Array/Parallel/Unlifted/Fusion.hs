-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Fusion
-- Copyright   : (c) [2001..2002] Manuel M T Chakravarty & Gabriele Keller
--		 (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (Rewrite Rules)
--
-- Description ---------------------------------------------------------------
--
-- Combinator-based array fusion
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Fusion (
  module Data.Array.Parallel.Base.Fusion,
  module Data.Array.Parallel.Unlifted.Flat.Fusion,
  module Data.Array.Parallel.Unlifted.Segmented.Fusion
) where

import Data.Array.Parallel.Base.Fusion
import Data.Array.Parallel.Unlifted.Flat.Fusion
import Data.Array.Parallel.Unlifted.Segmented.Fusion

