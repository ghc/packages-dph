-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Parallel.Text
-- Copyright   : (c) 2006 Roman Leshchinskiy
-- License     : see libraries/ndp/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Read\/Show instances for segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Parallel.Text ()
where

import Data.Array.Parallel.Base (
  Read(..), showsApp)
import Data.Array.Parallel.Unlifted.Parallel.UPSegd (
  UPSegd, lengthsUPSegd )

instance Show UPSegd where
  showsPrec k = showsApp k "toUPSegd" . lengthsUPSegd