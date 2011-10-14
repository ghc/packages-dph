{-# LANGUAGE
        CPP,
        TypeFamilies, MultiParamTypeClasses, 
        FlexibleInstances, FlexibleContexts,
        RankNTypes, ExistentialQuantification,
        StandaloneDeriving, TypeOperators #-}
{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | Define closures for each of the combinators the vectoriser uses.
module Data.Array.Parallel.Lifted.Combinators 
        ( emptyPP
        , singletonPP
        , lengthPP
        , replicatePP
        , indexPP
        , mapPP
        , appendPP
        , slicePP
        , concatPP

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
import Data.Array.Parallel.PArray.Sums
import Data.Array.Parallel.PArray
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

-- empty ----------------------------------------------------------------------
{-# INLINE_PA emptyPP #-}
emptyPP :: PA a => PArray a
emptyPP         = emptyPA 


-- singleton ------------------------------------------------------------------
{-# INLINE_PA singletonPP #-}
singletonPP :: PA a => a :-> PArray a
singletonPP     = closure1 singletonPA singletonPD_l


{-# INLINE singletonPD_l #-}
singletonPD_l :: PA a => Int -> PData a -> PData (PArray a)
singletonPD_l c xs 
        = replicatePD_l c (PInt $ U.replicate c 1) xs
        


-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
{-# INLINE_PA lengthPP #-}
lengthPP   :: PA a => PArray a :-> Int
lengthPP        = closure1 lengthPA lengthPD_l


{-# INLINE lengthPD_l #-}
lengthPD_l :: PA (PArray a)
           => Int -> PData (PArray a) -> PData Int
lengthPD_l _ (PNested vsegd _)
        = PInt $ U.takeLengthsOfVSegd vsegd


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
{-# INLINE_PA replicatePP #-}
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2 replicatePA replicatePD_l


{-# INLINE replicatePD_l #-}
replicatePD_l   :: PA a => Int -> PData Int -> PData a -> PData (PArray a)
replicatePD_l 0 _ _     = emptyPD
replicatePD_l c (PInt lens) pdata
 = let  segd    = U.lengthsToSegd lens
   in   mkPNested
                (U.replicate_s segd (U.enumFromTo 0 (c - 1)))
                lens
                (U.indicesSegd segd)
                (U.replicate c 0)
                (V.singleton pdata)


-- index ----------------------------------------------------------------------
-- | Lookup a single element from the souce array.
{-# INLINE_PA indexPP #-}
indexPP :: PA a => PArray a :-> Int :-> a
indexPP         = closure2 indexPA indexlPD


-- map ------------------------------------------------------------------------
{-# INLINE_PA mapPP #-}
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b

mapPP   = closure2 mapPA_v mapPD_l


{-# INLINE mapPA_v #-}
mapPA_v :: (PA a, PA b)
        => (a :-> b) -> PArray a -> PArray b
mapPA_v (Clo _fv fl env) (PArray n as) 
        = PArray n (fl n (replicatePD n env) as)


{-# INLINE mapPD_l #-}
mapPD_l :: (PA (a :-> b), PA a, PA b)
        => Int  -> PData (a :-> b) 
                -> PData (PArray a) -> PData (PArray b)

mapPD_l _ (AClo _fv fl envs) arg@(PNested vsegd _pdata)
 = let  argFlat         = concatPD arg
        c               = lengthPD argFlat

        -- TODO: rename this as unsafeDemoteToSegdOfVSegd.. it might overflow
        segd            = U.demoteToSegdOfVSegd vsegd

        envsReplicated  = replicatesPD segd envs
        arrResult       = fl c envsReplicated argFlat

  in    unconcatPD arg arrResult


-- append ---------------------------------------------------------------------
{-# INLINE_PA appendPP #-}
appendPP :: PA a => PArray a :-> PArray a :-> PArray a
appendPP        = closure2 appendPA appendPD_l

{-# INLINE appendPD_l #-}
appendPD_l :: PA a => Int -> PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendPD_l _ arr1 arr2
        = appendlPD arr1 arr2


-- slice ----------------------------------------------------------------------
{-# INLINE_PA slicePP #-}
slicePP :: PA a => Int :-> Int :-> PArray a :-> PArray a
slicePP         = closure3 slicePA slicePD_l


{-# INLINE slicePA #-}
slicePA :: PA a => Int -> Int -> PArray a -> PArray a
slicePA start len (PArray _ darr)
        = PArray len (extractPD darr start len)


{-# INLINE slicePD_l #-}
slicePD_l :: PA a => Int -> PData Int -> PData Int -> PData (PArray a) -> PData (PArray a)
slicePD_l _ sliceStarts sliceLens arrs
        = slicelPR sliceStarts sliceLens arrs


-- concat ---------------------------------------------------------------------
{-# INLINE_PA concatPP #-}
concatPP :: PA a => PArray (PArray a) :-> PArray a
concatPP
        = closure1 concatPA concatPD_l


{-# INLINE concatPD_l #-}
concatPD_l :: PA a => Int -> PData (PArray (PArray a)) -> PData (PArray a)
concatPD_l _ darr
        = concatlPD darr


-- Tuple ======================================================================
-- unzip ----------------------------------------------------------------------
{-# INLINE_PA unzipPP #-}
unzipPP :: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
unzipPP = closure1 unzipPA unzipPD_l


{-# INLINE unzipPD_l #-}
unzipPD_l :: (PA a, PA b) => Int -> PData (PArray (a, b)) -> PData (PArray a, PArray b)
unzipPD_l _ arr = unziplPD arr


-- Scalar =====================================================================
-- eq   -----------------------------------------------------------------------
{-# INLINE_PA eqPP_int #-}
eqPP_int        :: Int :-> Int :-> Int
eqPP_int        = closure2 (\x y -> intOfBool (x == y))
                          eqPP_int_l

{-# INLINE eqPP_int_l #-}
eqPP_int_l      :: Int -> PData Int -> PData Int -> PData Int
eqPP_int_l _ (PInt arr1) (PInt arr2)
        = PInt (U.zipWith (\x y -> intOfBool (x == y)) arr1 arr2)

{-# INLINE intOfBool #-}
intOfBool :: Bool -> Int
intOfBool True  = 1
intOfBool False = 0



-- plus -----------------------------------------------------------------------
{-# INLINE_PA plusPP_int #-}
plusPP_int      :: Int :-> Int :-> Int
plusPP_int
        = closure2 (+) plusPD_int_l


{-# INLINE plusPD_int_l #-}
plusPD_int_l    :: Int -> PData Int -> PData Int -> PData Int
plusPD_int_l _ (PInt arr1) (PInt arr2)
        = PInt (U.zipWith (+) arr1 arr2)


-- mult -----------------------------------------------------------------------
{-# INLINE_PA multPP_double #-}
multPP_double   :: Double :-> Double :-> Double
multPP_double
        = closure2 (*) multPD_double_l


{-# INLINE multPD_double_l #-}
multPD_double_l :: Int -> PData Double -> PData Double -> PData Double
multPD_double_l _ (PDouble arr1) (PDouble arr2)
        = PDouble (U.zipWith (*) arr1 arr2)


-- div -----------------------------------------------------------------------
{-# INLINE_PA divPP_int #-}
divPP_int   :: Int :-> Int :-> Int
divPP_int
        = closure2 div divPD_int_l


{-# INLINE divPD_int_l #-}
divPD_int_l :: Int -> PData Int -> PData Int -> PData Int
divPD_int_l _ (PInt arr1) (PInt arr2)
        = PInt (U.zipWith div arr1 arr2)


-- sum ------------------------------------------------------------------------
{-# INLINE_PA sumPP_double #-}
sumPP_double :: PArray Double :-> Double
sumPP_double
        = closure1 sumPA_double sumPA_l_double


{-# INLINE_PA sumPP_int #-}
sumPP_int :: PArray Int :-> Int
sumPP_int
        = closure1 sumPA_int sumPA_l_int
