module Data.Array.Parallel.Base.Closure (
  (:->)(..), closure, ($:)
) where

infixr 0 :->
infixr 0 $:

data a :-> b = forall e. Clo !(e -> a -> b) e

closure :: (e -> a -> b) -> e -> (a :-> b)
{-# INLINE closure #-}
closure = Clo

($:) :: (a :-> b) -> a -> b
{-# INLINE ($:) #-}
Clo f e $: x = f e x

