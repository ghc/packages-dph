module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  ($:), ($:^), closurePA
) where

import Data.Array.Parallel.Lifted.PArray

infixr 0 :->
infixr 0 $:, $:^

-- |The type of closures
--
data a :-> b = forall e. Clo !(PA e)
                             !(e -> a -> b)
                             !(PArray e -> PArray a -> PArray b)
                             e

-- |Closure application
--
($:) :: (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo _ f _ e $: a = f e a

-- |Arrays of closures (aka array closures)
--
data instance PArray (a :-> b) = forall e.
                                 AClo !(PA e)
                                      !(e -> a -> b)
                                      !(PArray e -> PArray a -> PArray b)
                                      !(PArray e)

-- |Lifted closure application
--
($:^) :: PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
AClo _ _ f es $:^ as = f es as

closure_lengthP :: PArray (a :-> b) -> Int
{-# INLINE closure_lengthP #-}
closure_lengthP (AClo pa _ _ es) = lengthP pa es

closure_replicateP :: Int -> (a :-> b) -> PArray (a :-> b)
{-# INLINE closure_replicateP #-}
closure_replicateP n (Clo pa f f' e) = AClo pa f f' (replicateP pa n e)

-- |Closure dictionary
closurePA :: PA (a :-> b)
closurePA = PA {
              lengthP    = closure_lengthP
            , replicateP = closure_replicateP
            }

