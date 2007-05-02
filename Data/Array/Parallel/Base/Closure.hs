module Data.Array.Parallel.Base.Closure (
  (:->)(..),
  closure, ($:),

  (:<->:)(..),
  to, from,
  identityIso, arrowIso
) where

infixr 0 :->
infixr 0 $:

infix  1 :<->:

data a :-> b = forall e. !(e -> a -> b) :$ e

closure :: (e -> a -> b) -> e -> (a :-> b)
{-# INLINE closure #-}
closure = (:$)

($:) :: (a :-> b) -> a -> b
{-# INLINE ($:) #-}
(f :$ e) $: x = f e x

data a :<->: b = (a -> b) :<->: (b -> a)

to :: a :<->: b -> a -> b
{-# INLINE to #-}
to (f :<->: _) = f

from :: a :<->: b -> b -> a
{-# INLINE from #-}
from (_ :<->: f) = f

identityIso :: a :<->: a
identityIso = id :<->: id

toClosure :: a :<->: a' -> b :<->: b' -> (a -> b) -> (a' :-> b')
{-# INLINE toClosure #-}
toClosure isoa isob f = (const (to isob . f . from isoa)) :$ ()

fromClosure :: a :<->: a' -> b :<->: b' -> (a' :-> b') -> (a -> b)
{-# INLINE fromClosure #-}
fromClosure isoa isob (f :$ e) = from isob . f e . to isoa

arrowIso :: forall a a'. a :<->: a'
         -> forall b b'. b :<->: b'
         -> (a -> b) :<->: (a' :-> b')
{-# INLINE arrowIso #-}
arrowIso isoa isob = toClosure isoa isob :<->: fromClosure isoa isob

