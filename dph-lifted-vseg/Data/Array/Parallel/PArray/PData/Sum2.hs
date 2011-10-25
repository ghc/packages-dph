#include "fusion-phases.h"
module Data.Array.Parallel.PArray.PData.Sum2 where

import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.PData.Int
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Array.Parallel.Base                 (Tag)
import Data.Array.Parallel.Unlifted             as U
import qualified Data.Vector                    as V
import Text.PrettyPrint
import Prelude                                  as P

-------------------------------------------------------------------------------
data instance PData (Sum2 a b)
        = PSum2  U.Sel2
                 (PData a)
                 (PData b)

data instance PDatas (Sum2 a b)
        = PSum2s (V.Vector U.Sel2)
                 (PDatas Tag)
                 (PDatas a)
                 (PDatas b)


-- PR -------------------------------------------------------------------------
-- This stuff isn't implemented yet.
nope str   = error $ "Data.Array.Parallel.PData.Void: no PR method for Sum2 " ++ str


instance (PR a, PR b) => PR (Sum2 a b)  where
  {-# INLINE_PDATA validPR #-}
  validPR _
        = True

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PSum2 (U.mkSel2 U.empty U.empty 0 0 (U.mkSelRep2 U.empty)) emptyPR emptyPR

  {-# INLINE_PDATA nfPR #-}
  nfPR (PSum2 sel xs ys)
        = sel `seq` nfPR xs `seq` nfPR ys `seq` ()

  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PSum2 sel xs ys)
        = U.length (U.tagsSel2 sel)

  {-# INLINE_PDATA replicatePR #-}
  replicatePR n aa
   = case aa of
      Alt2_1 x  
       -> PSum2 (U.mkSel2 (U.replicate n 0)
                          (U.enumFromStepLen 0 1 n)
                          n 0
                         (U.mkSelRep2 (U.replicate n 0)))
                (replicatePR n x)
                emptyPR
        
      Alt2_2 x
       -> PSum2 (U.mkSel2 (U.replicate n 1)
                          (U.enumFromStepLen 0 1 n)
                          0 n
                          (U.mkSelRep2 (U.replicate n 1)))
                emptyPR
                (replicatePR n x)    
                
  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR
        = nope "replicates"
                      
  {-# INLINE_PDATA indexPR #-}
  indexPR (PSum2 sel as bs) i
   = let !k = U.indicesSel2 sel U.!: i
     in  case U.tagsSel2 sel U.!: i of
             0 -> Alt2_1 (indexPR as k)
             _ -> Alt2_2 (indexPR bs k)
           
  {-# INLINE_PDATA indexlPR #-}
  indexlPR 
        = nope "indexl"
  
  {-# INLINE_PDATA extractPR #-}
  extractPR  
        = nope "extract"
  
  {-# INLINE_PDATA extractsPR #-}
  extractsPR 
        = nope "extracts"
        
  {-# INLINE_PDATA appendPR #-}
  appendPR (PSum2 sel1 as1 bs1)
           (PSum2 sel2 as2 bs2)
    = let !sel  = U.tagsToSel2
                $ U.tagsSel2 sel1 U.+:+ U.tagsSel2 sel2
      in  PSum2 sel (appendPR as1 as2)
                    (appendPR bs1 bs2)
        
  {-# INLINE_PDATA appendsPR #-}
  appendsPR
        = nope "appends"
        
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR
        = nope "packByTag"
  
  {-# INLINE_PDATA combine2PR #-}
  combine2PR 
        = nope "combine2"
        
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec
   = let tags   = V.convert $ V.map tagOfSum2 vec
         sel2   = U.tagsToSel2 tags
         
         -- TODO: Fix rubbish via-lists filtering.
         xs'    = fromVectorPR $ V.fromList $ [x | Alt2_1 x <- V.toList vec]
         ys'    = fromVectorPR $ V.fromList $ [x | Alt2_2 x <- V.toList vec]
         
     in  PSum2 sel2 xs' ys'
        
  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR
        = nope "toVector"

  -- PRR ----------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR 
        = PSum2s V.empty emptydPR emptydPR emptydPR

  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PSum2 sel2 xs ys)
        = PSum2s (V.singleton sel2)
                 (singletondPR (PInt (U.tagsSel2 sel2)))
                 (singletondPR xs)
                 (singletondPR ys)

  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PSum2s sel2s _ _ _)
        = V.length sel2s

  {-# INLINE_PDATA indexdPR #-}
  indexdPR  (PSum2s sel2s _ xss yss) ix
        = PSum2 (V.unsafeIndex sel2s ix)
                (indexdPR      xss   ix)
                (indexdPR      yss   ix)

  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PSum2s sels1 tagss1 xss1 yss1)
            (PSum2s sels2 tagss2 xss2 yss2)
   = PSum2s (sels1  V.++        sels2)
            (tagss1 `appenddPR` tagss2)
            (xss1   `appenddPR` xss2)
            (yss1   `appenddPR` yss2)

  {-# INLINE_PDATA concatdPR #-}
  concatdPR
        = nope "concatdPR"
                
  -- TODO: fix rubbish via-lists conversion.
  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
        = let   (sels, pdatas1, pdatas2) 
                        = P.unzip3 
                        $ [ (sel, pdata1, pdata2) 
                                    | PSum2 sel pdata1 pdata2 <- V.toList vec]
          in    PSum2s  (V.fromList sels)
                        (PInts $ V.map U.tagsSel2 $ V.fromList sels)
                        (fromVectordPR $ V.fromList pdatas1)
                        (fromVectordPR $ V.fromList pdatas2)
                
  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR 
        = nope "toVectordPR"


-- Pretty ---------------------------------------------------------------------

instance PprPhysical U.Sel2 where
 pprp sel2
  =   text "Sel2"
  $+$ (nest 4 $ vcat
       [ text "TAGS:   " <+> text (show $ U.toList $ U.tagsSel2 sel2)
       , text "INDICES:" <+> text (show $ U.toList $ U.indicesSel2 sel2)])


instance ( PprPhysical (PData a)
         , PprPhysical (PData b))
        => PprPhysical (PData (Sum2 a b)) where

 pprp (PSum2 sel pdatas1 pdatas2)
  =   text "PSum2"
  $+$ (nest 4 $ vcat
        [ pprp sel
        , text "ALTS0: " <+> pprp pdatas1
        , text "ALTS1: " <+> pprp pdatas2])


instance ( PprPhysical (PDatas a), PR a
         , PprPhysical (PDatas b), PR b)
        => PprPhysical (PDatas (Sum2 a b)) where

 pprp (PSum2s sels tagss pdatas1 pdatas2)
  =   text "PSum2s"
  $+$ (nest 4 $ vcat
        [ text "SELS:"          $+$ (nest 4 $ vcat $ P.map pprp $ V.toList sels)
        , text "PDATAS1:"       $$ (nest 4 $ pprp pdatas1)
        , text "PDATAS2:"       $$ (nest 4 $ pprp pdatas2)])


