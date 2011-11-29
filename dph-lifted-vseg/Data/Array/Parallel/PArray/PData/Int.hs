{-# OPTIONS_HADDOCK hide #-}
#include "fusion-phases.h"

-- | PR instance for Ints
module Data.Array.Parallel.PArray.PData.Int () where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint
import Prelude                                  as P
import Data.Array.Parallel.Pretty

-- PR -------------------------------------------------------------------------
instance PR Int where

  {-# NOINLINE validPR #-}
  validPR _
        = True

  {-# NOINLINE nfPR #-}
  nfPR (PInt xx)
        = xx `seq` ()

  {-# NOINLINE similarPR #-}
  similarPR  = (==)

  {-# NOINLINE coversPR #-}
  coversPR weak (PInt uarr) ix
   | weak       = ix <= U.length uarr
   | otherwise  = ix <  U.length uarr

  {-# NOINLINE pprpPR #-}
  pprpPR i
   =    int i

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PInt uarr)
   =    text "PInt" <+> pprp uarr


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PInt U.empty

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PInt (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PInt arr)
        = PInt (U.replicate_s segd arr)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PInt arr1) (PInt arr2)
        = PInt $ arr1 U.+:+ arr2

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PInt arr1) segd2 (PInt arr2)
        = PInt $ U.append_s segdResult segd1 arr1 segd2 arr2


  -- Projections --------------------------------                
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PInt uarr) 
        = U.length uarr

  {-# INLINE_PDATA indexPR #-}
  indexPR (PInt uarr) ix
        = uarr `U.unsafeIndex` ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PInts pvecs) (PInt srcs) (PInt ixs)
   = PInt $ U.zipWith get srcs ixs
   where get !src !ix
                = (pvecs `V.unsafeIndex` src) `U.unsafeIndex` ix

  {-# NOINLINE extractPR #-}
  extractPR (PInt arr) start len 
        = PInt (U.extract arr start len)

  {-# NOINLINE extractsPR #-}
  extractsPR (PInts arrs) ssegd
        = PInt $ U.extract_ss arrs ssegd



  -- Pack and Combine ---------------------------
  {-# NOINLINE packByTagPR #-}
  packByTagPR (PInt arr1) arrTags tag
        = PInt $ U.packByTag arr1 arrTags tag

  {-# NOINLINE combine2PR #-}
  combine2PR sel (PInt arr1) (PInt arr2)
        = PInt $ U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
        = PInt $U.fromList $ V.toList xx

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PInt arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PInts $ V.empty
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PInt pdata)
        = PInts $ V.singleton pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PInts vec)
        = V.length vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PInts vec) ix
        = PInt $ vec `V.unsafeIndex` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PInts xs) (PInts ys)
        = PInts $ xs V.++ ys
        
  {-# NOINLINE fromVectordPR #-}
  fromVectordPR vec
        = PInts $ V.map (\(PInt xs) -> xs) vec
        
  {-# NOINLINE toVectordPR #-}
  toVectordPR (PInts vec)
        = V.map PInt vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Int)
deriving instance Show (PDatas Int)

instance PprPhysical (U.Array Int) where
  pprp uarr 
   =    text (show $ U.toList uarr)

instance PprVirtual (PData Int) where
  pprv (PInt vec)
   = text (show $ U.toList vec)
