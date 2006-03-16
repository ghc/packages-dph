-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Base.Hyperstrict
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Hyperstrict types.
--
-- ---------------------------------------------------------------------------

module Data.Array.Parallel.Base.Hyperstrict (
  HS
) where

-- | The class of hyperstrict types. These are those types for which weak
-- head-normal form and normal form are the same, i.e., they are guaranteed to
-- contain no thunks once they are evaluated to whnf.
class HS a

instance HS ()
instance HS Bool
instance HS Char
instance HS Int
instance HS Float
instance HS Double

