{-# LANGUAGE
        CPP,
        TypeFamilies, MultiParamTypeClasses, 
        FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}
#include "fusion-phases-vseg.h"

-- | Define closures for each of the combinators the vectoriser uses.
module Data.Array.Parallel.Lifted.Combinators 
        ( singletonPP
        , lengthPP
        , replicatePP
        , indexPP
        , mapPP
        , slicePP

        -- * Tuple functions
        , unzipPP

        -- * Scalar functions
        -- TODO: Shift scalar functions should go into their own class.
        , eqPP_int
        , plusPP_int
        , multPP_double
        , divPP_int
        , sumPP_double, sumPP_int)
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Stream
import Data.Array.Parallel.PArray
import Debug.Trace
import Text.PrettyPrint

import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V

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


-- singleton ------------------------------------------------------------------
{-# INLINE_PA singletonPP #-}
singletonPP :: PA a => a :-> PArray a
singletonPP     = closure1 singletonPA singletonPA_l


{-# INLINE_PA singletonPA_l #-}
singletonPA_l :: PA a => Int -> PData a -> PData (PArray a)
singletonPA_l c xs 
        = replicatePA_l c (PInt $ U.replicate c 1) xs
        


-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
{-# INLINE_PA lengthPP #-}
lengthPP   :: PA a => PArray a :-> Int
lengthPP        = closure1 lengthPA lengthPA_l


{-# INLINE_PA lengthPA_l #-}
lengthPA_l :: PA (PArray a)
           => Int -> PData (PArray a) -> PData Int
lengthPA_l c (PNested vsegids pseglens psegstarts psegsrcs psegdata)
 = let  lens    = U.map (pseglens U.!:) vsegids
   in   PInt lens


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
{-# INLINE_PA replicatePP #-}
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2 replicatePA replicatePA_l


{-# INLINE_PA replicatePA_l #-}
replicatePA_l   :: PA a => Int -> PData Int -> PData a -> PData (PArray a)
replicatePA_l 0 _ _             = emptyPR
replicatePA_l c (PInt lens) pdata
 = let  segd    = U.lengthsToSegd lens
   in   PNested
        { pnested_vsegids       = U.replicate_s segd (U.enumFromTo 0 (c - 1))
        , pnested_pseglens      = lens
        , pnested_psegstarts    = U.indicesSegd segd
        , pnested_psegsrcs      = U.replicate c 0
        , pnested_psegdata      = V.singleton pdata }


-- index ----------------------------------------------------------------------
-- | Lookup a single element from the souce array.
{-# INLINE_PA indexPP #-}
indexPP :: PA a => PArray a :-> Int :-> a
indexPP         = closure2 indexPA indexlPR


-- map ------------------------------------------------------------------------
{-# INLINE_PA mapPP #-}
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b

mapPP   = closure2 mapPA_v mapPA_l


{-# INLINE_PA mapPA_v #-}
mapPA_v :: (PR a, PR b)
        => (a :-> b) -> PArray a -> PArray b
mapPA_v (Clo fv fl env) (PArray n as) 
        = PArray n (fl n (replicatePR n env) as)


{-# INLINE_PA mapPA_l #-}
mapPA_l :: (PR (a :-> b), PR a, PR b)
        => Int  -> PData (a :-> b) 
                -> PData (PArray a) -> PData (PArray b)

mapPA_l n (AClo fv fl envs) arg@(PNested vsegids pseglens psegstarts psegsrcs psegdata)
 = let  argFlat         = concatPR arg
        c               = lengthPR argFlat

        vseglens        = U.map (pseglens   U.!:) vsegids
        envsReplicated  = replicatesPR vseglens envs
        arrResult       = fl c envsReplicated argFlat

  in    unconcatPR arg arrResult


-- slice ----------------------------------------------------------------------
{-# INLINE_PA slicePP #-}
slicePP :: PA a => Int :-> Int :-> PArray a :-> PArray a
slicePP         = closure3 slicePA slicePA_l


{-# INLINE_PA slicePA #-}
slicePA :: PA a => Int -> Int -> PArray a -> PArray a
slicePA start len (PArray _ darr)
        = PArray len (extractPR darr start len)


{-# INLINE_PA slicePA_l #-}
slicePA_l :: PA a => Int -> PData Int -> PData Int -> PData (PArray a) -> PData (PArray a)
slicePA_l _ sliceStarts sliceLens arrs
        = slicelPR sliceStarts sliceLens arrs


-- append ---------------------------------------------------------------------
-- {-# INLINE_PA appendPP #-}
-- appendPP :: PA a => PArray a :-> PArray a :-> PArray a
-- appendPP        = closure2 appendPA appendPA_l


-- Tuple ======================================================================
-- unzip ----------------------------------------------------------------------
{-# INLINE_PA unzipPP #-}
unzipPP :: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
unzipPP         = closure1 unzipPA unzipPA_l


-- Scalar =====================================================================
-- eq   -----------------------------------------------------------------------
{-# INLINE_PA eqPP_int #-}
eqPP_int        :: Int :-> Int :-> Int
eqPP_int        = closure2 (\x y -> intOfBool (x == y))
                          eqPP_int_l

{-# INLINE_PA eqPP_int_l #-}
eqPP_int_l      :: Int -> PData Int -> PData Int -> PData Int
eqPP_int_l _ (PInt arr1) (PInt arr2)
        = PInt (U.zipWith (\x y -> intOfBool (x == y)) arr1 arr2)

{-# INLINE intOfBool #-}
intOfBool :: Bool -> Int
intOfBool True  = 1
intOfBool False = 0



-- plus -----------------------------------------------------------------------
{-# INLINE_PA plusPP_int #-}
plusPP_int       :: Int :-> Int :-> Int
plusPP_int       = closure2 (+) plusPA_int_l


{-# INLINE_PA plusPA_int_l #-}
plusPA_int_l    :: Int -> PData Int -> PData Int -> PData Int
plusPA_int_l _ (PInt arr1) (PInt arr2)
        = PInt (U.zipWith (+) arr1 arr2)


-- mult -----------------------------------------------------------------------
{-# INLINE_PA multPP_double #-}
multPP_double   :: Double :-> Double :-> Double
multPP_double = closure2 (*) multPA_double_l


{-# INLINE_PA multPA_double_l #-}
multPA_double_l :: Int -> PData Double -> PData Double -> PData Double
multPA_double_l c (PDouble arr1) (PDouble arr2)
        = PDouble (U.zipWith (*) arr1 arr2)


-- div -----------------------------------------------------------------------
{-# INLINE_PA divPP_int #-}
divPP_int   :: Int :-> Int :-> Int
divPP_int = closure2 div divPA_int_l


{-# INLINE_PA divPA_int_l #-}
divPA_int_l :: Int -> PData Int -> PData Int -> PData Int
divPA_int_l c (PInt arr1) (PInt arr2)
        = PInt (U.zipWith div arr1 arr2)


-- sum ------------------------------------------------------------------------
{-# INLINE_PA sumPP_double #-}
sumPP_double :: PArray Double :-> Double
sumPP_double    = closure1 sumPA_double sumPA_l_double


{-# INLINE_PA sumPA_double #-}
sumPA_double   :: PArray Double -> Double
sumPA_double (PArray _ (PDouble xs))
        = U.sum xs


{-# INLINE_PA sumPP_int #-}
sumPP_int :: PArray Int :-> Int
sumPP_int    = closure1 sumPA_int sumPA_l_int


{-# INLINE_PA sumPA_int #-}
sumPA_int   :: PArray Int  -> Int
sumPA_int (PArray _ (PInt xs))
        = U.sum xs

