-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Base
-- Copyright   : (c) [2006,2007]        Roman Leshchinsskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : non-portable (unboxed values and GHC libraries)
--
--
-- Basic functionality, imported by most modules.
--

module Data.Array.Parallel.Base (
  -- * Debugging infrastructure
  module Data.Array.Parallel.Base.Debug,

  -- * Data constructor tags
  module Data.Array.Parallel.Base.Util,

  -- * Utils for defining Read\/Show instances.
  module Data.Array.Parallel.Base.Text,

  -- * Tracing infrastructure
  module Data.Array.Parallel.Base.DTrace,
  module Data.Array.Parallel.Base.TracePrim,
  
  -- * ST monad re-exported from GHC
  ST(..), runST
) where

import Data.Array.Parallel.Base.Debug
import Data.Array.Parallel.Base.Util
import Data.Array.Parallel.Base.Text
import Data.Array.Parallel.Base.DTrace
import Data.Array.Parallel.Base.TracePrim

import GHC.ST (ST(..), runST)

