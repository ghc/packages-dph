module Data.Array.Parallel.Base.Rebox (
  Rebox(..), Box(..)
) where

import Data.Array.Parallel.Base.Hyperstrict

import GHC.Base   (Int(..))
import GHC.Float  (Float(..), Double(..))

class Rebox a where
  rebox :: a -> a

instance Rebox () where
  {-# INLINE [0] rebox #-}
  rebox () = ()

instance Rebox Bool where
  {-# INLINE [0] rebox #-}
  rebox True = True
  rebox False = False

instance Rebox Int where
  {-# INLINE [0] rebox #-}
  rebox (I# i#) = id (I# i#)

instance Rebox Float where
  {-# INLINE [0] rebox #-}
  rebox (F# f#) = F# f#

instance Rebox Double where
  {-# INLINE [0] rebox #-}
  rebox (D# d#) = D# d#

instance (Rebox a, Rebox b) => Rebox (a :*: b) where
  {-# INLINE [0] rebox #-}
  rebox (x :*: y) = rebox x :*: rebox y

instance Rebox a => Rebox (MaybeS a) where
  {-# INLINE [0] rebox #-}
  rebox NothingS  = NothingS
  rebox (JustS x) = JustS (rebox x)

newtype Box a = Box a

instance Rebox (Box a) where
  {-# INLINE [0] rebox #-}
  rebox (Box a) = Box a

