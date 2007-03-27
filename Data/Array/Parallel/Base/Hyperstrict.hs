-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Parallel.Base.Hyperstrict
-- Copyright   :  (c) 2006 Roman Leshchinskiy
-- License     :  see libraries/ndp/LICENSE
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
  (:*:)(..), EitherS(..),

  -- * Injection and projection functions
  fstS, sndS, pairS, unpairS,

  -- * Currying
  curryS, uncurryS,

  -- * Strict Maybe
  MaybeS(..), maybeS, fromMaybeS,

  -- * Lazy wrapper
  Lazy(..),

  -- * Class of hyperstrict types
  HS
) where

infixl 2 :*:

-- |Strict pair
data (:*:) a b = !a :*: !b deriving(Eq,Ord,Show,Read)

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
data EitherS a b = LeftS !a | RightS !b

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

data Lazy a = Lazy a deriving(Eq, Ord, Show, Read)

instance Functor Lazy where
  fmap f (Lazy x) = Lazy (f x)

-- | The class of hyperstrict types. These are those types for which weak
-- head-normal form and normal form are the same.
-- That is, once they are evaluated to WHNF, they are guaranteed to
-- contain no thunks 
class HS a

instance HS ()
instance HS Bool
instance HS Char
instance HS Int
instance HS Float
instance HS Double

instance (HS a, HS b) => HS (a :*: b)
instance (HS a, HS b) => HS (EitherS a b)
instance HS a => HS (MaybeS a)


