{-# LANGUAGE
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}

-- | Define closures for each of the combinators the vectoriser uses.
--
--   For each combinator:
--    The *PA_v version is the "vectorised" version that has had its parameters
--    closure converted. For first-order functions, the *PA_v version is
--    identical to the standard *PA version from D.A.P.PArray, so we can 
--    just use that directly.
--
--    The *PA_l version is the "lifted" version that works on arrays of arrays.
--    Each of these functions also takes an integer as its first argument. 
--    This is the "lifting context" that says now many element to expect in 
--    each of the argument arrays. 
--
--    The *PP version contains both the vectorised and lifted versions wrapped
--    up in a closure. The code produced by the vectoriser uses the *PP
--    versions directly.
--
module Data.Array.Parallel.Lifted.Combinators where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray

import qualified Data.Array.Parallel.Unlifted     as U

-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
lengthPP   :: PA a => PArray a :-> Int
lengthPP = closure1 lengthPA lengthPA_l


lengthPA_l :: PJ m (PArray a)
	   => Int -> PData m (PArray a) -> PData Sized Int

lengthPA_l c da
 = let	PNestedS segd _	= restrictPJ c da
   in	error "lengthPA_l"


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2 replicatePA replicatePA_l


replicatePA_l   :: PA a 
                => Int -> PData m1 Int -> PData m2 a -> PData Sized (PArray a)
replicatePA_l = error "replciatePA_l"


-- map ------------------------------------------------------------------------
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b
mapPP 	= closure2 mapPA_v mapPA_l


mapPA_v :: (PR a, PR b)
	=> (a :-> b) -> PArray a -> PArray b

mapPA_v (Clo fv fl env) (PArray n as) 
	= PArray n (fl n (repeatPE env) as)


mapPA_l :: (PJ m1 (a :-> b), PJ m2 (PArray a))
	=> Int 	-> PData m1 (a :-> b) 
		-> PData m2 (PArray a) -> PData Sized (PArray b)

mapPA_l n clo arr2
 = let	PNestedS segd2 d2	= restrictPJ n arr2
   in	error "mapPA_l"


-- index ----------------------------------------------------------------------
indexPP :: PA a => PArray a :-> Int :-> a
indexPP	= closure2 indexPA indexPA_l


indexPA_l :: (PJ m1 (PArray a), PJ m2 Int)
	=> Int -> PData m1 (PArray a) -> PData m2 Int -> PData Sized a

indexPA_l n src ixs
 = case indexlPJ n src (restrictPJ n ixs) of
	PArray _ d	-> d


-- plus -----------------------------------------------------------------------
plusPP	:: Int :-> Int :-> Int
plusPP	 = closure2 plusPA_v plusPA_l

plusPA_v  :: Int -> Int -> Int
plusPA_v = (+)

plusPA_l  :: (PJ m1 Int, PJ m2 Int)
        => Int -> PData m1 Int -> PData m2 Int -> PData Sized Int
plusPA_l n d1 d2
 = let PIntS vec1 = restrictPJ n d1
       PIntS vec2 = restrictPJ n d2
   in  PIntS (U.zipWith (+) vec1 vec2)

