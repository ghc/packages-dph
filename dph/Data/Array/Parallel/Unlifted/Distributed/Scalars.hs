-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Unlifted.Distributed.Scalars
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Distributed scalars.
--

module Data.Array.Parallel.Unlifted.Distributed.Scalars (
  unitD, scalarD,

  orD, andD, sumD
) where

import Data.Array.Parallel.Unlifted.Distributed.Gang (
  Gang, seqGang)
import Data.Array.Parallel.Unlifted.Distributed.Types (
  DT, Dist, unitD)
import Data.Array.Parallel.Unlifted.Distributed.Combinators (
  mapD, foldD)

-- unitD reexported from Types

-- | Distribute a scalar.
scalarD :: DT a => Gang -> a -> Dist a
scalarD g x = mapD (seqGang g) (const x) (unitD g)

-- |
--
orD :: Gang -> Dist Bool -> Bool
orD g = foldD g (||)

-- |
--
andD :: Gang -> Dist Bool -> Bool
andD g = foldD g (&&)

-- |
--
sumD :: (Num a, DT a) => Gang -> Dist a -> a
sumD g = foldD g (+)


