#include "fusion-phases.h"

-- | This is the API used by the vectoriser.
--   The vectoriser wants a slightly different interface to the one used 
--   natively by the library. This module performs the impedance matching.
module Data.Array.Parallel.Prim 
        -- Core types
        ( PArray(..), PData, PRepr, PA(..), PR(..)

        -- Array Functions
        , emptyPD
        , replicatePD
        , packByTagPD
        , combine2PD

        -- Scalar primitives
        , Scalar(..)
        , scalar_map
        , scalar_zipWith
        , scalar_zipWith3

        -- Types used in the generic representation
        , Void, void, fromVoid, pvoid
        , punit
        , Wrap(..)
        , Sum2(..), Sum3(..)
        
        -- Closures, and closure functions
        , (:->)(..)
        , closure,              ($:)
        , liftedClosure,        liftedApply
        , closure1, closure2, closure3
        
        -- Selectors
        , Sel2
        , tagsSel2
        , replicateSel2#
        , elementsSel2_0#
        , elementsSel2_1#

        -- Scalar constructors
        , PArray_Int#,          PArray_Double#
        , emptyPA_Int#,         emptyPA_Double#
        , replicatePA_Int#,     replicatePA_Double#
        , packByTagPA_Int#,     packByTagPA_Double#
        , combine2PA_Int#,      combine2PA_Double#

        -- Tuple functions
        , tup2, tup3)
where
import Data.Array.Parallel.PArray.PData.Base   (PArray(..), PData, PR(..))
import Data.Array.Parallel.PArray.PData
        ( pvoid, punit)

import Data.Array.Parallel.PArray.PRepr 
        ( PRepr, PA(..)
        , emptyPA, replicatePA, packByTagPA, combine2PA)
        
import Data.Array.Parallel.PArray.Scalar
        ( Scalar(..))

import Data.Array.Parallel.PArray.Types
        ( Void, void, fromVoid
        , Wrap(..)
        , Sum2(..), Sum3(..))
        
import Data.Array.Parallel.Lifted.Closure
        ( (:->)(..))

import Data.Array.Parallel.Prelude.Tuple
        ( tup2, tup3)

import Data.Array.Parallel.Base                         (Tag, intToTag)
import qualified Data.Array.Parallel.Unlifted           as U
import qualified Data.Array.Parallel.PArray.Scalar      as Scalar
import qualified Data.Array.Parallel.Lifted.Closure     as C
import GHC.Exts

-- Array functions ------------------------------------------------------------
emptyPD :: PA a => PData a
emptyPD = emptyPA


replicatePD :: PA a => Int# -> a -> PData a
replicatePD i# x 
        = replicatePA (I# i#) x
        

packByTagPD :: PA a => PData a -> Int# -> U.Array Tag -> Int# -> PData a
packByTagPD xs _ tags tag#
        = packByTagPA xs tags (I# tag#)


combine2PD :: PA a => Int# -> U.Sel2 -> PData a -> PData a -> PData a
combine2PD len# sel xs ys
        = combine2PA sel xs ys


-- Closures -------------------------------------------------------------------
-- The vectoriser wants versions of these functions that take unboxed
-- integers for the first argument of the lifted function.

-- | Construct a closure.
{-# INLINE closure #-}
closure :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> e
        -> (a :-> b)

closure fv fl e 
 = Clo fv 
         (\(I# c) v x -> fl c v x)
         e


-- | Apply a closure.
{-# INLINE ($:) #-}
($:)    = (C.$:)


-- | Construct a lifted closure.
{-# INLINE liftedClosure #-}
liftedClosure
        :: forall a b e
        .  PA e
        => (e -> a -> b)
        -> (Int# -> PData e -> PData a -> PData b)
        -> PData e
        -> PData (a :-> b)

liftedClosure fv fl es
 = C.AClo fv 
        (\(I# c) v x -> fl c v x)
        es
        
-- | Apply a lifted closure.
{-# INLINE liftedApply #-}
liftedApply :: Int# -> PData (a :-> b) -> PData a -> PData b
liftedApply n# arr xs
        = C.liftedApply (I# n#) arr xs


{-# INLINE closure1 #-}
closure1 :: forall a b
         .  (a -> b)
         -> (PArray a -> PArray b)
         -> (a :-> b)
closure1 fv fl
 = let  fl' :: Int -> PData a -> PData b
        fl' (I# c#) pdata 
         = case fl (PArray c# pdata) of
                 PArray _ pdata' -> pdata'
                        
   in   C.closure1 fv fl'


{-# INLINE closure2 #-}
closure2 :: forall a b c. PA a
         => (a -> b -> c)
         -> (PArray a -> PArray b -> PArray c)
         -> (a :-> b :-> c)
closure2 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c
        fl' (I# c#) pdata1 pdata2
         = case fl (PArray c# pdata1) (PArray c# pdata2) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure2 fv fl'


{-# INLINE closure3 #-}
closure3 :: forall a b c d.  (PA a, PA b)
         => (a -> b -> c -> d)
         -> (PArray a -> PArray b -> PArray c -> PArray d)
         -> (a :-> b :-> c :-> d)
closure3 fv fl
 = let  fl' :: Int -> PData a -> PData b -> PData c -> PData d
        fl' (I# c#) pdata1 pdata2 pdata3
         = case fl (PArray c# pdata1) (PArray c# pdata2) (PArray c# pdata3) of
                 PArray _ pdata' -> pdata'
                
   in   C.closure3 fv fl'


-- Selector functions ---------------------------------------------------------
-- The vectoriser wants versions of these that take unboxed integers
-- for some arguments.
type Sel2       = U.Sel2


{-# INLINE replicateSel2# #-}
replicateSel2# :: Int# -> Int# -> Sel2
replicateSel2# n# tag#
  = U.mkSel2
         (U.replicate n (intToTag tag))
         (U.enumFromStepLen 0 1 n)
         (if tag == 0 then n else 0)
         (if tag == 0 then 0 else n)
         (U.mkSelRep2 (U.replicate n (intToTag tag)))
  where
    n   = I# n#
    tag = I# tag#


{-# INLINE pickSel2# #-}
pickSel2# :: Sel2 -> Int# -> U.Array Bool
pickSel2# sel tag#
        = U.pick (U.tagsSel2 sel) (intToTag (I# tag#))


{-# INLINE tagsSel2 #-}
tagsSel2 :: Sel2 -> U.Array Tag
tagsSel2 = U.tagsSel2


{-# INLINE elementsSel2_0# #-}
elementsSel2_0# :: Sel2 -> Int#
elementsSel2_0# sel
        = case U.elementsSel2_0 sel of { I# n# -> n# }


{-# INLINE elementsSel2_1# #-}
elementsSel2_1# :: Sel2 -> Int#
elementsSel2_1# sel
        = case U.elementsSel2_1 sel of { I# n# -> n# }


-- Scalar functions -----------------------------------------------------------
{-# INLINE scalar_map #-}
scalar_map 
        :: (Scalar a, Scalar b) 
        => (a -> b) -> PArray a -> PArray b

scalar_map      = Scalar.map


{-# INLINE scalar_zipWith #-}
scalar_zipWith
        :: (Scalar a, Scalar b, Scalar c)
        => (a -> b -> c) -> PArray a -> PArray b -> PArray c

scalar_zipWith  = Scalar.zipWith


{-# INLINE scalar_zipWith3 #-}
scalar_zipWith3
        :: (Scalar a, Scalar b, Scalar c, Scalar d)
        => (a -> b -> c -> d) -> PArray a -> PArray b -> PArray c -> PArray d

scalar_zipWith3 = Scalar.zipWith3


-- Int functions --------------------------------------------------------------
type PArray_Int# = U.Array Int


replicatePA_Int# :: Int# -> Int# -> PArray_Int#
replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)
{-# INLINE_PA replicatePA_Int# #-}


emptyPA_Int# :: PArray_Int#
emptyPA_Int# = U.empty
{-# INLINE_PA emptyPA_Int# #-}


{-# RULES

"replicatePA_Int#" forall n# i#.
  replicatePA_Int# n# i# = U.replicate (I# n#) (I# i#)

 #-}


packByTagPA_Int# :: a
packByTagPA_Int#    = error "Data.Array.Parallel.Prim: 'packByTagPA_Int#' not implemented"


combine2'PA_Int# :: PArray_Int# -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2'PA_Int# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Int# #-}


combine2PA_Int#
        :: Int#
        -> PArray_Int# -> PArray_Int#
        -> PArray_Int# -> PArray_Int# -> PArray_Int#
combine2PA_Int# _ sel _ xs ys = combine2'PA_Int# sel xs ys
{-# INLINE_PA combine2PA_Int# #-}


-- Double functions -----------------------------------------------------------
type PArray_Double# = U.Array Double


replicatePA_Double# :: Int# -> Double# -> PArray_Double#
replicatePA_Double# n# d# = U.replicate (I# n#) (D# d#)
{-# INLINE_PA replicatePA_Double# #-}


emptyPA_Double# :: PArray_Double#
emptyPA_Double# = U.empty
{-# INLINE_PA emptyPA_Double# #-}


packByTagPA_Double# :: a
packByTagPA_Double# = error "Data.Array.Parallel.Prim: 'packByTagPA_Double#' not implemented"


combine2'PA_Double#
        :: PArray_Int#
        -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2'PA_Double# sel xs ys = U.combine (U.map (== 0) sel) xs ys
{-# INLINE_PA combine2'PA_Double# #-}


combine2PA_Double#
        :: Int#
        -> PArray_Int# -> PArray_Int#
        -> PArray_Double# -> PArray_Double# -> PArray_Double#
combine2PA_Double# _ sel _ xs ys = combine2'PA_Double# sel xs ys
{-# INLINE_PA combine2PA_Double# #-}

