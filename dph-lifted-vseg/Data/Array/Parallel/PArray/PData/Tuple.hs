{-# LANGUAGE
        CPP,
        TypeFamilies,
        FlexibleInstances, FlexibleContexts,
        StandaloneDeriving, ExplicitForAll,
        MultiParamTypeClasses #-}
#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Tuple 
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.Base
import qualified Data.Vector                    as V
import Text.PrettyPrint

data instance PData (a, b)
        = PTuple2 (PData a) (PData b)


deriving instance (Show (PData a), Show (PData b)) 
        => Show (PData (a, b))


instance (PprPhysical (PData a), PprPhysical (PData b))
        => PprPhysical (PData (a, b)) where
 pprp   (PTuple2 xs ys)
        = text "PTuple2 " <> vcat [pprp xs, pprp ys]


instance ( PR a, PR b, Show a, Show b
         , PprVirtual (PData a), PprVirtual (PData b))
        => PprVirtual (PData (a, b)) where
 pprv   (PTuple2 xs ys)
        = text $ show 
        $ zip (V.toList $ toVectorPR xs) 
              (V.toList $ toVectorPR ys)


instance (PR a, PR b) => PR (a, b) where
  {-# INLINE_PDATA validPR #-}
  validPR (PTuple2 xs ys)
        =  checkEq "validPR[Tuple2]" "array length mismatch" 
                (lengthPR xs) (lengthPR ys)
        $  validPR xs && validPR ys

  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple2 emptyPR emptyPR

  {-# INLINE_PDATA nfPR #-}
  nfPR (PTuple2 arr1 arr2)
        = nfPR arr1 `seq` nfPR arr2 `seq` ()

  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple2 arr1 _)
        = lengthPR arr1

  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y)
        = PTuple2 (replicatePR len x)
                  (replicatePR len y)

  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple2 arr1 arr2)
        = PTuple2 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)

  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple2 arr1 arr2) ix
        = (indexPR arr1 ix, indexPR arr2 ix)

  {-# INLINE_PDATA indexlPR #-}
  indexlPR c (PNested uvsegd psegdata) ixs
   = let (xs, ys)       = V.unzip $ V.map (\(PTuple2 xs' ys') -> (xs', ys')) psegdata 
         xsArr          = PNested uvsegd xs
         ysArr          = PNested uvsegd ys
     in  PTuple2  (indexlPR c xsArr ixs) (indexlPR c ysArr ixs)

  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple2 arr1 arr2) start len
        = PTuple2 (extractPR arr1 start len) 
                  (extractPR arr2 start len)

  {-# INLINE_PDATA extractsPR #-}
  extractsPR arrs ussegd
   = let (xs, ys)       = V.unzip $ V.map (\(PTuple2 xs' ys') -> (xs', ys')) arrs
     in  PTuple2 (extractsPR xs ussegd)
                 (extractsPR ys ussegd)

  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple2 arr11 arr12) (PTuple2 arr21 arr22)
        = PTuple2 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)

  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PTuple2 arrs11 arrs12) segd2 (PTuple2 arrs21 arrs22)
        = PTuple2 (appendsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendsPR segdResult segd1 arrs12 segd2 arrs22)

  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple2 arr1 arr2) tags tag
        = PTuple2 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)

  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple2 xs1 ys1) (PTuple2 xs2 ys2)
        = PTuple2 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)

  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys)       = V.unzip vec
     in  PTuple2 (fromVectorPR xs)
                 (fromVectorPR ys)

  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PTuple2 xs ys)
        = V.zip  (toVectorPR xs)
                 (toVectorPR ys)

  {-# INLINE_PDATA fromUArrayPR #-}
  fromUArrayPR
   = error "fromUArrayPR[Tuple]: put in Scalar class, need U.Elt dictionary"
   
  {-# INLINE_PDATA toUArrayPR #-}
  toUArrayPR
   = error "toUArrayPR[Tuple]: put in Scalar class, need U.Elt dictionary"
   

-- | Zip a pair of arrays into an array of pairs.
--   The two arrays must have the same length, else `error`. 
{-# INLINE_PA zipPA #-}
zipPA :: PArray a -> PArray b -> PArray (a, b)
zipPA (PArray n1 xs) (PArray n2 ys)
        | n1 == n2
        = PArray n1 (PTuple2 xs ys)
        
        | otherwise
        = error "Data.Array.Parallel.PArray.zipPA: arrays are not the same length"


-- | Unzip an array of pairs into a pair of arrays.
{-# INLINE_PA unzipPA #-}
unzipPA :: PArray (a, b) -> (PArray a, PArray b)
unzipPA (PArray n (PTuple2 xs ys))
        = (PArray n xs, PArray n ys)


{-# INLINE_PA unzipPA_l #-}
unzipPA_l :: (PR a, PR b)
          => Int -> PData (PArray (a, b)) -> PData (PArray a, PArray b)
unzipPA_l _ (PNested uvsegd psegdata)
 = let  (xsdata, ysdata)        = V.unzip $ V.map (\(PTuple2 xs ys) -> (xs, ys)) psegdata
   in   PTuple2 (PNested uvsegd xsdata)
                (PNested uvsegd ysdata)

