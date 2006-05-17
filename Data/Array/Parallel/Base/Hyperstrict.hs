-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Base.Hyperstrict
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Hyperstrict types.
--
-- ---------------------------------------------------------------------------

module Data.Array.Parallel.Base.Hyperstrict (

  -- * Strict pairs and sums
  (:*:)(..), (:+:)(..),

  -- * Injection and projection functions
  fstS, sndS, pairS, unpairS,

  -- * Currying
  curryS, uncurryS,

  -- * Strict Maybe
  MaybeS(..), maybeS, fromMaybeS,

  -- * Class of hyperstrict types
  HS
) where

infixl 1 :+:
infixl 2 :*:

-- |Strict pair
data (:*:) a b = !a :*: !b deriving(Eq,Ord,Show)

fstS :: a :*: b -> a
fstS (x :*: _) = x

sndS :: a :*: b -> b
sndS (_ :*: y) = y

pairS :: (a,b) -> a :*: b
pairS = uncurry (:*:)

unpairS :: a :*: b -> (a,b)
unpairS (x :*: y) = (x,y)

curryS :: (a :*: b -> c) -> a -> b -> c
curryS f x y = f (x :*: y) 

uncurryS :: (a -> b -> c) -> a :*: b -> c
uncurryS f (x :*: y) = f x y

-- |Strict sum
data (:+:) a b = Inl !a | Inr !b

-- |Strict Maybe
data MaybeS a = NothingS | JustS !a

instance Functor MaybeS where
  fmap f (JustS x) = JustS (f x)
  fmap f NothingS  = NothingS

-- MaybeS doesn't seem to be a proper monad. With the obvious definition we'd
-- get:
--
--   return _|_ >>= const Nothing  =  _|_  /=  const Nothing _|_

maybeS :: b -> (a -> b) -> MaybeS a -> b
maybeS b f (JustS a) = f a
maybeS b f NothingS  = b

fromMaybeS :: a -> MaybeS a -> a
fromMaybeS x (JustS y) = y
fromMaybeS x NothingS  = x

-- | The class of hyperstrict types. These are those types for which weak
-- head-normal form and normal form are the same, i.e., they are guaranteed to
-- contain no thunks once they are evaluated to whnf.
class HS a

instance HS ()
instance HS Bool
instance HS Char
instance HS Int
instance HS Float
instance HS Double

instance (HS a, HS b) => HS (a :*: b)
instance (HS a, HS b) => HS (a :+: b)
instance HS a => HS (MaybeS a)


