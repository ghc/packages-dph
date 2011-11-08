#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Double where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import qualified Data.Array.Parallel.Unlifted   as U
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import Text.PrettyPrint


data instance PData Double
        = PDouble !(U.Array Double)

data instance PDatas Double
        = PDoubles !(V.Vector (U.Array Double))


-- PR -------------------------------------------------------------------------
instance PR Double where

  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA nfPR #-}
  nfPR (PDouble xx)
        = xx `seq` ()

  {-# INLINE_PDATA similarPR #-}
  similarPR  = (==)

  {-# INLINE_PDATA coversPR #-}
  coversPR weak (PDouble uarr) ix
   | weak       = ix <= U.length uarr
   | otherwise  = ix <  U.length uarr

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PDouble vec)
   =   text "PDouble"
   <+> text (show $ U.toList vec)


  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PDouble U.empty

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len x
        = PDouble (U.replicate len x)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR segd (PDouble arr)
        = PDouble (U.replicate_s segd arr)

  {-# INLINE_PDATA appendPR #-}
  appendPR (PDouble arr1) (PDouble arr2)
        = PDouble (arr1 U.+:+ arr2)

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PDouble arr1) segd2 (PDouble arr2)
        = PDouble $ U.append_s segdResult segd1 arr1 segd2 arr2


  -- Projections --------------------------------                
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PDouble uarr)
        = U.length uarr

  {-# INLINE_PDATA indexPR #-}
  indexPR (PDouble arr) ix
        = arr `VU.unsafeIndex` ix

  {-# INLINE_PDATA indexlPR #-}
  indexlPR arr@(PNested vsegd (PDoubles vecpdatas)) (PInt ixs)
   = PDouble $ U.zipWith get vsegids ixs
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
  extractPR (PDouble arr) start len 
        = PDouble (U.extract arr start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR (PDoubles vecpdatas) ussegd
   = let segsrcs        = U.sourcesSSegd ussegd
         segstarts      = U.startsSSegd  ussegd
         seglens        = U.lengthsSSegd ussegd
     in  PDouble (uextracts vecpdatas segsrcs segstarts seglens)
                

  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PDouble arr1) arrTags tag
        = PDouble $ U.packByTag arr1 arrTags tag

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PDouble arr1) (PDouble arr2)
        = PDouble (U.combine2 (U.tagsSel2 sel)
                           (U.repSel2  sel)
                           arr1 arr2)


  -- Conversions --------------------------------
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR xx
        = PDouble (U.fromList $ V.toList xx)

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PDouble arr)
        = V.fromList $ U.toList arr


  -- PDatas -------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PDoubles $ V.empty
        
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PDouble pdata)
        = PDoubles $ V.singleton pdata
        
  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PDoubles vec)
        = V.length vec
        
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PDoubles vec) ix
        = PDouble $ V.unsafeIndex vec ix

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PDoubles xs) (PDoubles ys)
        = PDoubles $ xs V.++ ys
        
  {-# INLINE_PDATA concatdPR #-}
  concatdPR vecs
        = PDoubles
                $ V.concat $ V.toList
                $ V.map (\(PDoubles xs) -> xs) vecs
                
  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = PDoubles $ V.map (\(PDouble xs) -> xs) vec
        
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PDoubles vec)
        = V.map PDouble vec


-- Show -----------------------------------------------------------------------
deriving instance Show (PData  Double)
deriving instance Show (PDatas Double)

instance PprVirtual (PData Double) where
  pprv (PDouble vec)
   = text (show $ U.toList vec)

