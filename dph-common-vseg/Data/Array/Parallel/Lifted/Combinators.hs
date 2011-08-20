{-# LANGUAGE
        CPP,
	TypeFamilies, MultiParamTypeClasses, 
	FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}
#include "fusion-phases-vseg.h"

-- | Define closures for each of the combinators the vectoriser uses.
module Data.Array.Parallel.Lifted.Combinators 
        ( lengthPP
        , replicatePP
        , mapPP
	, indexPP
--        , unzipPP

        -- TODO: Shift scalar functions should go into their own class.
--        , multPP_double
--        , sumPP_double
        , plusPP_int
--        , plus3PP_int
          )
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray
import Debug.Trace
import Text.PrettyPrint

import qualified Data.Array.Parallel.Unlifted     as U

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


-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
{-# INLINE_PA lengthPP #-}
lengthPP   :: PA a => PArray a :-> Int
lengthPP        = closure1 lengthPA lengthPA_l


{-# INLINE_PA lengthPA_l #-}
lengthPA_l :: PA (PArray a)
	   => Int -> PData (PArray a) -> PData Int
lengthPA_l c (PNested vsegids pseglens psegstarts psegsrcs psegdata)
 = let	lens    = U.map (pseglens U.!:) vsegids
   in   PInt lens


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
{-# INLINE_PA replicatePP #-}
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2 replicatePA replicatePA_l


{-# INLINE_PA replicatePA_l #-}
replicatePA_l   :: PA a 
                => Int -> PData Int -> PData a -> PData (PArray a)
replicatePA_l = error "replciatePA_l"


-- map ------------------------------------------------------------------------
{-# INLINE_PA mapPP #-}
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b

mapPP 	= closure2 mapPA_v mapPA_l


{-# INLINE_PA mapPA_v #-}
mapPA_v :: (PR a, PR b)
	=> (a :-> b) -> PArray a -> PArray b
mapPA_v (Clo fv fl env) (PArray n as) 
	= PArray n (fl n (replicatePR n env) as)


{-# INLINE_PA mapPA_l #-}
mapPA_l :: (PR (a :-> b), PR a, PR b)
	=> Int 	-> PData (a :-> b) 
		-> PData (PArray a) -> PData (PArray b)

mapPA_l n (AClo fv fl envs) arg@(PNested vsegids pseglens psegstarts psegsrcs psegdata)
 = let  argFlat         = concatPR arg
        c               = lengthPR argFlat

        vseglens        = U.map (pseglens   U.!:) vsegids
        envsReplicated  = replicatesPR vseglens envs
        arrResult       = fl c envsReplicated argFlat

  in    unconcatPR arg arrResult


-- index ----------------------------------------------------------------------
{-# INLINE_PA indexPP #-}
indexPP :: (PA a, PprPhysical (PData a)) => PArray a :-> Int :-> a
indexPP	= closure2 indexPA indexlPR

{-

-- Tuple ======================================================================
-- unzip ----------------------------------------------------------------------
{-# INLINE_PA unzipPP #-}
unzipPP :: PArray (a, b) :-> (PArray a, PArray b)
unzipPP         = closure1 unzipPA unzipPA_l



-- Scalar =====================================================================
-- sum ------------------------------------------------------------------------
{-# INLINE_PA sumPP_double #-}
sumPP_double :: PArray Double :-> Double
sumPP_double    = closure1 sumPA sumPA_l


{-# INLINE_PA sumPA #-}
sumPA   :: PArray Double -> Double
sumPA (PArray _ (PDoubleS xs))
        = U.sum xs


{-# INLINE_PA sumPA_l #-}
sumPA_l :: forall m1. PJ m1 (PArray Double)
        => Int -> PData m1 (PArray Double) -> PData Sized Double
sumPA_l c xss
 = let  PNestedS segd (PDoubleS xs)   = restrictPJ c xss
   in   PDoubleS (U.sum_s segd xs)
-}


-- plus -----------------------------------------------------------------------
{-# INLINE_PA plusPP_int #-}
plusPP_int	 :: Int :-> Int :-> Int
plusPP_int	 = closure2 (+) plusPA_int_l


{-# INLINE_PA plusPA_int_l #-}
plusPA_int_l    :: Int -> PData Int -> PData Int -> PData Int
plusPA_int_l n (PInt arr1) (PInt arr2)
        = PInt (U.zipWith (+) arr1 arr2)

{-
-- mult -----------------------------------------------------------------------
{-# INLINE_PA multPP_double #-}
multPP_double   :: Double :-> Double :-> Double
multPP_double = closure2 (*) multPA_double_l


{-# INLINE_PA multPA_double_l #-}
multPA_double_l :: (PJ m1 Double, PJ m2 Doubl
         => Int -> PData m1 Double -> PData m2 Double -> PData Sized Double
multPA_double_l n d1 d2
 = let PDoubleS vec1 = restrictPJ n d1
       PDoubleS vec2 = restrictPJ n d2
   in  PDoubleS (U.zipWith (*) vec1 vec2)
-}

