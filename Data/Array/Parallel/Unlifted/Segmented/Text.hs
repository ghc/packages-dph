-----------------------------------------------------------------------------
-- |
-- Module      : Data.Array.Parallel.Unlifted.Flat.Text
-- Copyright   : (c) 2006         Manuel M T Chakravarty & Roman Leshchinskiy
-- License     : see libraries/base/LICENSE
-- 
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   : internal
-- Portability : portable
--
-- Description ---------------------------------------------------------------
--
--  Read/Show instances for segmented unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Segmented.Text ()
where

import Data.Array.Parallel.Base (
  Read(..), showsApp, readApp)
import Data.Array.Parallel.Unlifted.Flat (
  UA)
import Data.Array.Parallel.Unlifted.Segmented.SUArr (
  USegd, SUArr, fromUSegd, toUSegd)
import Data.Array.Parallel.Unlifted.Segmented.Basics (
  fromSU, toSU)

instance Show USegd where
  showsPrec k = showsApp k "toUSegd" . fromUSegd

instance Read USegd where
  readPrec = fmap toUSegd (readApp "toUSegd")

instance (Show e, UA e) => Show (SUArr e) where
  showsPrec k = showsApp k "toSU" . fromSU

instance (Read e, UA e) => Read (SUArr e) where
  readPrec = fmap toSU (readApp "toSU")

