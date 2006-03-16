module Data.Array.Parallel.Base.Generics
where

data Unit = Unit
data (:+:) a b = Inl !a | Inr !b
data (:*:) a b = !a :*: !b
