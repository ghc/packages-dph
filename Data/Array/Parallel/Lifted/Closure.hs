module Data.Array.Parallel.Lifted.Closure (
  (:->)(..), PArray(..),
  mkClosure, mkClosureP, ($:), ($:^)
) where

import Data.Array.Parallel.Lifted.PArray

infixr 0 :->
infixr 0 $:, $:^

-- |The type of closures
--
data a :-> b = forall e. PA e => Clo !(e -> a -> b)
                                     !(PArray e -> PArray a -> PArray b)
                                     e

-- |Closure construction
--
mkClosure :: forall a b e. 
             PA e => (e -> a -> b)
                  -> (PArray e -> PArray a -> PArray b)
                  -> e -> (a :-> b)
{-# INLINE mkClosure #-}
mkClosure = Clo

-- |Closure application
--
($:) :: forall a b. (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo f _ e $: a = f e a

-- |Arrays of closures (aka array closures)
--
data instance PArray (a :-> b)
  = forall e. PA e => AClo !(e -> a -> b)
                           !(PArray e -> PArray a -> PArray b)
                           !(PArray e)

-- |Lifted closure construction
--
mkClosureP :: forall a b e.
              PA e => (e -> a -> b)
                   -> (PArray e -> PArray a -> PArray b)
                   -> PArray e -> PArray (a :-> b)
{-# INLINE mkClosureP #-}
mkClosureP = AClo

-- |Lifted closure application
--
($:^) :: forall a b. PArray (a :-> b) -> PArray a -> PArray b
{-# INLINE ($:^) #-}
AClo _ f es $:^ as = f es as

instance (PA a, PA b) => PA (a :-> b) where
  {-# INLINE lengthPA #-}
  lengthPA      (AClo _ _ es) = lengthPA es
  {-# INLINE replicatePA #-}
  replicatePA n (Clo  f f' e) = AClo f f' (replicatePA n e)

