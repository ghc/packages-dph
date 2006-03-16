module Data.Array.Parallel.Base.Generics (
  Unit(..), (:+:)(..), (:*:)(..)
) where

import Data.Array.Parallel.Base.Hyperstrict

data Unit = Unit
data (:+:) a b = Inl !a | Inr !b
data (:*:) a b = !a :*: !b

instance HS Unit
instance (HS a, HS b) => HS (a :*: b)

-- do we need this?
instance (HS a, HS b) => HS (a :+: b)

