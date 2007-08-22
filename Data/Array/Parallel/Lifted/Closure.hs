module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  mkClosure, mkClosureP, ($:), ($:^),
  dPA_Clo
) where

import Data.Array.Parallel.Lifted.PArray

infixr 0 :->
infixr 0 $:, $:^

-- |The type of closures
--
data a :-> b = forall e. Clo (PA e)
                             !(e -> a -> b)
                             !(PArray e -> PArray a -> PArray b)
                             e

-- |Closure construction
--
mkClosure :: forall a b e. 
             PA e -> (e -> a -> b)
                  -> (PArray e -> PArray a -> PArray b)
                  -> e -> (a :-> b)
{-# INLINE mkClosure #-}
mkClosure = Clo

-- |Closure application
--
($:) :: forall a b. (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo _ f _ e $: a = f e a

-- |Arrays of closures (aka array closures)
--
data instance PArray (a :-> b)
  = forall e. AClo (PA e)
                   !(e -> a -> b)
                   !(PArray e -> PArray a -> PArray b)
                   !(PArray e)

-- |Lifted closure construction
--
mkClosureP :: forall a b e.
              PA e -> (e -> a -> b)
                   -> (PArray e -> PArray a -> PArray b)
                   -> PArray e -> PArray (a :-> b)
{-# INLINE mkClosureP #-}
mkClosureP = AClo

-- |Lifted closure application
--
($:^) :: forall a b. PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
AClo _ _ f es $:^ as = f es as

type instance PRepr (a :-> b) = a :-> b

dPA_Clo :: PA a -> PA b -> PA (a :-> b)
{-# INLINE dPA_Clo #-}
dPA_Clo _ _ = PA {
                lengthPA    = lengthPA_Clo
              , replicatePA = replicatePA_Clo
              , toPRepr     = id
              , fromPRepr   = id
              }

{-# INLINE lengthPA_Clo #-}
lengthPA_Clo (AClo pa f f' es) = lengthPA pa es

{-# INLINE replicatePA_Clo #-}
replicatePA_Clo n# (Clo pa f f' e) = AClo pa f f' (replicatePA pa n# e)

