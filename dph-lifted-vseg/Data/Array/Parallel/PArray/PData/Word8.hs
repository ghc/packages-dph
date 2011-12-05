{-# OPTIONS_HADDOCK hide #-}
#include "fusion-phases.h"

-- | PR instance for Word8.
module Data.Array.Parallel.PArray.PData.Word8 where
import Data.Array.Parallel.PArray.PData.Base
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import Text.PrettyPrint
import Prelude                                  as P
import Data.Word
import Data.Array.Parallel.Pretty

-------------------------------------------------------------------------------
data instance PData Word8
        = PWord8  !(U.Array Word8)

data instance PDatas Word8
        = PWord8s !(U.Arrays Word8)


-- PR -------------------------------------------------------------------------
instance PR Word8 where

  {-# NOINLINE validPR #-}
  validPR _
        = True

  {-# NOINLINE nfPR #-}
  nfPR (PWord8 xx)
        = xx `seq` ()

  {-# NOINLINE similarPR #-}
  similarPR  = (==)

  {-# NOINLINE coversPR #-}
  coversPR weak (PWord8 uarr) ix
   | weak       = ix <= U.length uarr
   | otherwise  = ix <  U.length uarr

  {-# NOINLINE pprpPR #-}
  pprpPR i
   =    int (fromIntegral i)

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PWord8 uarr)
   =    text "PWord8" <+> pprp uarr


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PWord8 U.empty

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PWord8 (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PWord8 arr)
        = PWord8 (U.replicate_s segd arr)
                
  {-# INLINE_PDATA appendPR #-}
  appendPR (PWord8 arr1) (PWord8 arr2)
        = PWord8 $ arr1 U.+:+ arr2

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PWord8 arr1) segd2 (PWord8 arr2)
        = PWord8 $ U.append_s segdResult segd1 arr1 segd2 arr2


  -- Projections --------------------------------                
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PWord8 uarr) 
        = U.length uarr

  {-# INLINE_PDATA indexPR #-}
  indexPR (PWord8 uarr) ix
        = uarr `U.unsafeIndex` ix

  {-# INLINE_PDATA indexsPR #-}
  indexsPR (PWord8s pvecs) (PInt srcs) (PInt ixs)
   = PWord8 $ U.zipWith (U.unsafeIndex2s pvecs) srcs ixs

  {-# INLINE_PDATA extractPR #-}
  extractPR (PWord8 arr) start len 
        = PWord8 (U.extract arr start len)

  {-# INLINE_PDATA extractssPR #-}
  extractssPR (PWord8s arrs) ssegd
        = PWord8 $ U.unsafeExtracts_ass ssegd arrs

  {-# INLINE_PDATA extractvsPR #-}
  extractvsPR (PWord8s arrs) vsegd
        = PWord8 $ U.unsafeExtracts_avs vsegd arrs


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PWord8 arr1) arrTags tag
        = PWord8 $ U.packByTag arr1 arrTags tag

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PWord8 arr1) (PWord8 arr2)
        = PWord8 $ U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2


  -- Conversions --------------------------------
  {-# NOINLINE fromVectorPR #-}
  fromVectorPR xx
        = PWord8 $U.fromList $ V.toList xx

  {-# NOINLINE toVectorPR #-}
  toVectorPR (PWord8 arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PWord8s $ U.emptys
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PWord8 pdata)
        = PWord8s $ U.singletons pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PWord8s arrs)
        = U.lengths arrs
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PWord8s arrs) ix
        = PWord8 $ arrs `U.unsafeIndexs` ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PWord8s xs) (PWord8s ys)
        = PWord8s $ xs `U.appends` ys
                                
  {-# NOINLINE fromVectordPR #-}
  fromVectordPR pdatas
        = PWord8s 
        $ U.fromVectors
        $ V.map (\(PWord8 xs) -> xs) pdatas
        
  {-# NOINLINE toVectordPR #-}
  toVectordPR (PWord8s vec)
        = V.map PWord8 $ U.toVectors vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Word8)
deriving instance Show (PDatas Word8)

instance PprPhysical (U.Array Word8) where
  pprp uarr 
   =    text (show $ U.toList uarr)

instance PprVirtual (PData Word8) where
  pprv (PWord8 vec)
   = text (show $ U.toList vec)

