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
-- Interface to the Base modules
--

module Data.Array.Parallel.Base (
  module Data.Array.Parallel.Base.Debug,
  module Data.Array.Parallel.Base.Hyperstrict,
  module Data.Array.Parallel.Base.Text,
  module Data.Array.Parallel.Base.Rebox,

  ST(..), runST
) where

import Data.Array.Parallel.Base.Debug
import Data.Array.Parallel.Base.Hyperstrict
import Data.Array.Parallel.Base.Text
import Data.Array.Parallel.Base.Rebox

import GHC.ST (ST(..), runST)

