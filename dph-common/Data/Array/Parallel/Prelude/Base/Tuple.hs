module Data.Array.Parallel.Prelude.Base.Tuple (
  tup2, tup3
) where

import Data.Array.Parallel.Lifted.Combinators
import Data.Array.Parallel.Lifted.Repr
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.Lifted.PArray

tup2 :: PA a -> PA b -> a :-> b :-> (a,b)
{-# INLINE tup2 #-}
tup2 pa pb = closure2 pa (,) zipPA#

tup3 :: PA a -> PA b -> PA c -> a :-> b :-> c :-> (a,b,c)
{-# INLINE tup3 #-}
tup3 pa pb pc = closure3 pa pb (,,) zip3PA#

