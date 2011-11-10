{-# OPTIONS_HADDOCK hide #-}
#include "fusion-phases.h"

-- | PR instance for Word8.
module Data.Array.Parallel.PArray.PData.Word8 where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import Text.PrettyPrint
import Prelude                                  as P
import Data.Word
import Data.Array.Parallel.Pretty

-------------------------------------------------------------------------------
data instance PData Word8
        = PWord8 !(U.Array Word8)

data instance PDatas Word8
        = PWord8s !(V.Vector (U.Array Word8))


-- PR -------------------------------------------------------------------------
instance PR Word8 where

  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA nfPR #-}
  nfPR (PWord8 xx)
        = xx `seq` ()

  {-# INLINE_PDATA similarPR #-}
  similarPR  = (==)

  {-# INLINE_PDATA coversPR #-}
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
        = uarr U.!: ix

  {-# INLINE_PDATA indexlPR #-}
  indexlPR (PNested vsegd (PWord8s vecpdatas)) (PInt ixs)
   = PWord8 $ U.zipWith get vsegids ixs
   where
         -- Unbox these vectors outside the get loop.
         !vsegids       = U.takeVSegidsRedundantOfVSegd vsegd
         !ssegd         = U.takeSSegdRedundantOfVSegd vsegd
         !psegsrcids    = U.sourcesSSegd ssegd
         !psegstarts    = U.startsSSegd  ssegd

         -- Lookup a single element from a virtual segment.
         get !vsegid !ix
          = let !psegsrcid       = psegsrcids `VU.unsafeIndex` vsegid
                !psegvec         = vecpdatas  `V.unsafeIndex` psegsrcid
                !psegstart       = psegstarts `VU.unsafeIndex` vsegid
                !elemIx          = psegstart + ix
                !elemVal         = psegvec    `VU.unsafeIndex` elemIx
            in  elemVal

  {-# INLINE_PDATA extractPR #-}
  extractPR (PWord8 arr) start len 
        = PWord8 (U.extract arr start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR (PWord8s vecpdatas) ussegd
   = let segsrcs        = U.sourcesSSegd ussegd
         segstarts      = U.startsSSegd  ussegd
         seglens        = U.lengthsSSegd ussegd
     in  PWord8 $ U.extract_ss vecpdatas segsrcs segstarts seglens

  {-# INLINE_PDATA bpermutePR #-}
  bpermutePR (PWord8 arr) indices
        = PWord8 $ U.bpermute arr indices


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
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR xx
        = PWord8 $U.fromList $ V.toList xx

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PWord8 arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PWord8s $ V.empty
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PWord8 pdata)
        = PWord8s $ V.singleton pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PWord8s vec)
        = V.length vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PWord8s vec) ix
        = PWord8 $ V.unsafeIndex vec ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PWord8s xs) (PWord8s ys)
        = PWord8s $ xs V.++ ys
        
  {-# INLINE_PDATA concatdPR #-}
  concatdPR vecs
        = PWord8s $ V.concat $ V.toList
                $ V.map (\(PWord8s xs) -> xs) vecs
                                
  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PWord8s $ V.map (\(PWord8 xs) -> xs) vec
        
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PWord8s vec)
        = V.map PWord8 vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Word8)
deriving instance Show (PDatas Word8)

instance PprPhysical (U.Array Word8) where
  pprp uarr 
   =    text (show $ U.toList uarr)

instance PprVirtual (PData Word8) where
  pprv (PWord8 vec)
   = text (show $ U.toList vec)

