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
--  Read/Show instances for flat unlifted arrays.
--
-- Todo ----------------------------------------------------------------------
--

module Data.Array.Parallel.Unlifted.Flat.Text ()
where

import Data.Array.Parallel.Unlifted.Flat.UArr (
  UA, UArr)
import Data.Array.Parallel.Unlifted.Flat.Basics (
  fromU, toU)
import Data.Array.Parallel.Base (
  Read(..), showsApp, readApp)

instance (Show e, UA e) => Show (UArr e) where
  showsPrec k = showsApp k "toU" . fromU

instance (Read e, UA e) => Read (UArr e) where
  readPrec = fmap toU (readApp "toU")

