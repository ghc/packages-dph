-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Base
-- Copyright   : (c) 2006         Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
-- Description ---------------------------------------------------------------
--
-- Interface to the Arr modules
--

module Data.Array.Parallel.Arr (
  module Data.Array.Parallel.Arr.BBArr,
  module Data.Array.Parallel.Arr.BUArr,
  module Data.Array.Parallel.Arr.Prim
) where

import Data.Array.Parallel.Arr.BBArr
import Data.Array.Parallel.Arr.BUArr
import Data.Array.Parallel.Arr.Prim

