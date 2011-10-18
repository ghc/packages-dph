{-# OPTIONS -fno-spec-constr #-}
#include "fusion-phases.h"

-- | Polymorphic closures that take PA dictionaries.
--
--   The vectoriser produces code that uses some of these combinators directly,
--   and the rest are called from D.A.P.Parallel.
--
--   All of the combinators defined here are polymorphic, and take PA dictionaries.
--   Combinators that are specific to a certain element type, like Int, are defined
--   directly in the corresponding prelude module, eg D.A.P.Prelude.Int.
--
module Data.Array.Parallel.Lifted.Combinators 
        ( -- * Conversions
          fromPArrayPP
        , toPArrayPP
        , fromNestedPArrayPP
        
        -- * Constructors
        , emptyPP
        , singletonPP
        , replicatePP
        , appendPP

        -- * Projections
        , lengthPP
        , indexPP
        , slicePP

        -- * Traversals
        , mapPP

        -- * Filtering
        , filterPP

        -- * Concatenation
        , concatPP

        -- * Tuple functions
        , unzipPP)
where
import Data.Array.Parallel.Lifted.Closure
import Data.Array.Parallel.PArray.PData         as PA
import Data.Array.Parallel.PArray.PRepr         as PA
import Data.Array.Parallel.PArray.Sums          as PA
import Data.Array.Parallel.PArray               as PA
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import GHC.Exts

nope    = error "Data.Array.Parallel.Lifted.Combinators: can't use unvectorised definition"


-- Conversions ================================================================
-- The following identity functions are used as the vectorised versions of the
-- functions that convert between the source level array type [:a:] and the 
-- PArray type which is used in the library. 

-- | Identity function, used as the vectorised version of fromPArrayP.
fromPArrayPP :: PA a => PArray a :-> PArray a
fromPArrayPP            = closure1 (\x -> x) (\_ xs -> xs)
{-# INLINE fromPArrayPP #-}


-- | Identity function, used as the vectorised version of toPArrayP.
toPArrayPP :: PA a => PArray a :-> PArray a
toPArrayPP              = closure1 (\x -> x) (\_ xs -> xs)
{-# INLINE toPArrayPP #-}


-- | Identity function, used as the vectorised version of fromNesterPArrayP
fromNestedPArrayPP :: PA a => (PArray (PArray a) :-> PArray (PArray a))
fromNestedPArrayPP      = closure1 (\xs -> xs) (\_ xss -> xss)
{-# INLINE fromNestedPArrayPP #-}



-- Operators ==================================================================
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

-- Constructors ===============================================================
-- empty ----------------------------------------------------------------------
{-# INLINE_PA emptyPP #-}
emptyPP :: PA a => PArray a
emptyPP         = PA.empty


-- singleton ------------------------------------------------------------------
{-# INLINE_PA singletonPP #-}
singletonPP :: PA a => a :-> PArray a
singletonPP     = closure1 PA.singleton singletonlPA


{-# INLINE singletonlPA #-}
singletonlPA :: PA a => Int -> PData a -> PData (PArray a)
singletonlPA c xs 
        = replicatelPA' c (PInt $ U.replicate c 1) xs


-- replicate ------------------------------------------------------------------
-- | Produce a new array by replicating a single element the given number of times.
{-# INLINE_PA replicatePP #-}
replicatePP     :: PA a => Int :-> a :-> PArray a
replicatePP     = closure2 PA.replicate replicatelPA'


{-# INLINE replicatelPA' #-}
replicatelPA'   :: PA a => Int -> PData Int -> PData a -> PData (PArray a)
replicatelPA' 0 _ _     = emptyPA
replicatelPA' c (PInt lens) pdata
 = let  segd    = U.lengthsToSegd lens
   in   mkPNested
                (U.replicate_s segd (U.enumFromTo 0 (c - 1)))
                lens
                (U.indicesSegd segd)
                (U.replicate c 0)
                (V.singleton pdata)


-- append ---------------------------------------------------------------------
{-# INLINE_PA appendPP #-}
appendPP :: PA a => PArray a :-> PArray a :-> PArray a
appendPP        = closure2 PA.append appendlPA'

{-# INLINE appendlPA' #-}
appendlPA' :: PA a => Int -> PData (PArray a) -> PData (PArray a) -> PData (PArray a)
appendlPA' _ arr1 arr2
        = appendlPA arr1 arr2



-- Projections ================================================================
-- length ---------------------------------------------------------------------
-- | Take the number of elements in an array.
{-# INLINE_PA lengthPP #-}
lengthPP   :: PA a => PArray a :-> Int
lengthPP        = closure1 PA.length lengthlPA'


{-# INLINE lengthlPA' #-}
lengthlPA' :: PA (PArray a)
           => Int -> PData (PArray a) -> PData Int
lengthlPA' _ (PNested vsegd _)
        = PInt $ U.takeLengthsOfVSegd vsegd


-- index ----------------------------------------------------------------------
-- | Lookup a single element from the souce array.
{-# INLINE_PA indexPP #-}
indexPP :: PA a => PArray a :-> Int :-> a
indexPP         = closure2 PA.index indexlPA


-- slice ----------------------------------------------------------------------
{-# INLINE_PA slicePP #-}
slicePP :: PA a => Int :-> Int :-> PArray a :-> PArray a
slicePP         = closure3 PA.slice slicelPA'


{-# INLINE slicelPA' #-}
slicelPA' :: PA a => Int -> PData Int -> PData Int -> PData (PArray a) -> PData (PArray a)
slicelPA' _ sliceStarts sliceLens arrs
        = slicelDD sliceStarts sliceLens arrs


-- Traversals =================================================================
-- map ------------------------------------------------------------------------
{-# INLINE_PA mapPP #-}
mapPP   :: (PA a, PA b) 
        => (a :-> b) :-> PArray a :-> PArray b

mapPP   = closure2 mapPA_v mapPD_l


{-# INLINE mapPA_v #-}
mapPA_v :: (PA a, PA b)
        => (a :-> b) -> PArray a -> PArray b
mapPA_v (Clo _fv fl env) (PArray n# as) 
        = PArray n# (fl (I# n#) (replicatePA (I# n#) env) as)


{-# INLINE mapPD_l #-}
mapPD_l :: (PA (a :-> b), PA a, PA b)
        => Int  -> PData (a :-> b) 
                -> PData (PArray a) -> PData (PArray b)

mapPD_l _ (AClo _fv fl envs) arg@(PNested vsegd _pdata)
 = let  argFlat         = concatPA arg
        c               = lengthPA argFlat

        -- TODO: rename this as unsafeDemoteToSegdOfVSegd.. it might overflow
        segd            = U.demoteToSegdOfVSegd vsegd

        envsReplicated  = replicatesPA segd envs
        arrResult       = fl c envsReplicated argFlat

  in    unconcatPA arg arrResult

{-
mapPD_l c fs ass
        =   unconcatPD ass 
        $   liftedApply c (replicatesPD (unsafeDemoteSegdPD ass) fs)
        $   concatPD ass
-}      


-- Filtering =================================================================
-- | Extract the elements from an array that match the given predicate.
filterPP :: PA a => (a :-> Bool) :-> PArray a :-> PArray a
{-# INLINE filterPP #-}
filterPP = nope


-- Concatenation ==============================================================
{-# INLINE_PA concatPP #-}
concatPP :: PA a => PArray (PArray a) :-> PArray a
concatPP = closure1 PA.concat concatPD_l


{-# INLINE concatPD_l #-}
concatPD_l :: PA a => Int -> PData (PArray (PArray a)) -> PData (PArray a)
concatPD_l _ darr
        = concatlPA darr


-- Tuple Functions ============================================================
-- unzip ----------------------------------------------------------------------
{-# INLINE_PA unzipPP #-}
unzipPP :: (PA a, PA b) => PArray (a, b) :-> (PArray a, PArray b)
unzipPP = closure1 PA.unzip unzipPD_l


{-# INLINE unzipPD_l #-}
unzipPD_l :: (PA a, PA b) => Int -> PData (PArray (a, b)) -> PData (PArray a, PArray b)
unzipPD_l _ arr = unziplPD arr

