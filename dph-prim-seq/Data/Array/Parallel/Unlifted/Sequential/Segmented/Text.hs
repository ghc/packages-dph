-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Sequential.Flat.Text
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

module Data.Array.Parallel.Unlifted.Sequential.Segmented.Text ()
where

import Data.Array.Parallel.Base (
  Read(..), showsApp, readApp)
import Data.Array.Parallel.Unlifted.Sequential.Flat (
  UA)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.SUArr (
  USegd, SUArr, lengthsUSegd, toUSegd)
import Data.Array.Parallel.Unlifted.Sequential.Segmented.Basics (
  fromSU, toSU)

instance Show USegd where
  showsPrec k = showsApp k "toUSegd" . lengthsUSegd

instance Read USegd where
  readPrec = fmap toUSegd (readApp "toUSegd")

instance (Show e, UA e) => Show (SUArr e) where
  showsPrec k = showsApp k "toSU" . fromSU

instance (Read e, UA e) => Read (SUArr e) where
  readPrec = fmap toSU (readApp "toSU")

