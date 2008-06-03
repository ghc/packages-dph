-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Base
-- Copyright   : (c) [2006..2007] Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
-- Interface to the Arr modules
--

module Data.Array.Parallel.Arr (
  module Data.Array.Parallel.Arr.BBArr,
  module Data.Array.Parallel.Arr.BUArr
) where

import Data.Array.Parallel.Arr.BBArr
import Data.Array.Parallel.Arr.BUArr

