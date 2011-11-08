#include "fusion-phases.h"

module Data.Array.Parallel.PArray.PData.Tuple 
        ( PData(..),    PDatas(..)
        , zip,          zipPD
        , zipl,         ziplPD
        , unzip,        unzipPD
        , unzipl,       unziplPD)
where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.Base
import Text.PrettyPrint
import GHC.Exts
import Prelude hiding (zip, unzip)
import qualified Data.Vector                    as V
import qualified Prelude                        as P

-------------------------------------------------------------------------------
data instance PData (a, b)
        = PTuple2  (PData a) (PData b)

data instance PDatas (a, b)
        = PTuple2s (PDatas a) (PDatas b)


-- PR -------------------------------------------------------------------------
instance (PR a, PR b) => PR (a, b) where

  {-# INLINE_PDATA validPR #-}
  validPR (PTuple2 xs ys)
        = validPR xs && validPR ys


  {-# INLINE_PDATA nfPR #-}
  nfPR (PTuple2 arr1 arr2)
        = nfPR arr1 `seq` nfPR arr2 `seq` ()


  {-# INLINE_PDATA similarPR #-}
  similarPR (x1, y1) (x2, y2)
        =  similarPR x1 x2
        && similarPR y1 y2


  {-# INLINE_PDATA coversPR #-}
  coversPR weak (PTuple2 arr1 arr2) ix
        =  coversPR weak arr1 ix
        && coversPR weak arr2 ix

  {-# NOINLINE pprpPR #-}
  pprpPR (x, y)
        = text "Tuple2 " <> vcat [pprpPR x, pprpPR y]
        

  {-# NOINLINE pprpDataPR #-}
  pprpDataPR (PTuple2 xs ys)
        = text "PTuple2 " <> vcat [pprpDataPR xs, pprpDataPR ys]

  -- Constructors -------------------------------
  {-# INLINE_PDATA emptyPR #-}
  emptyPR
        = PTuple2 emptyPR emptyPR


  {-# INLINE_PDATA replicatePR #-}
  replicatePR len (x, y)
        = PTuple2 (replicatePR len x)
                  (replicatePR len y)


  {-# INLINE_PDATA replicatesPR #-}
  replicatesPR lens (PTuple2 arr1 arr2)
        = PTuple2 (replicatesPR lens arr1)
                  (replicatesPR lens arr2)


  {-# INLINE_PDATA appendPR #-}
  appendPR (PTuple2 arr11 arr12) (PTuple2 arr21 arr22)
        = PTuple2 (arr11 `appendPR` arr21)
                  (arr12 `appendPR` arr22)


  {-# INLINE_PDATA appendsPR #-}
  appendsPR segdResult segd1 (PTuple2 arrs11 arrs12) segd2 (PTuple2 arrs21 arrs22)
        = PTuple2 (appendsPR segdResult segd1 arrs11 segd2 arrs21)
                  (appendsPR segdResult segd1 arrs12 segd2 arrs22)


  -- Projections ---------------------------------
  {-# INLINE_PDATA lengthPR #-}
  lengthPR (PTuple2 arr1 _) 
        = lengthPR arr1
  
  {-# INLINE_PDATA indexPR #-}
  indexPR (PTuple2 arr1 arr2) ix
        = (indexPR arr1 ix, indexPR arr2 ix)


  {-# INLINE_PDATA indexlPR #-}
  indexlPR (PNested uvsegd (PTuple2s xs ys)) ixs
   = let xsArr  = PNested uvsegd xs
         ysArr  = PNested uvsegd ys
     in  PTuple2  (indexlPR xsArr ixs) (indexlPR ysArr ixs)


  {-# INLINE_PDATA extractPR #-}
  extractPR (PTuple2 arr1 arr2) start len
        = PTuple2 (extractPR arr1 start len) 
                  (extractPR arr2 start len)


  {-# INLINE_PDATA extractsPR #-}
  extractsPR (PTuple2s xs ys) ussegd
   =    PTuple2  (extractsPR xs ussegd)
                 (extractsPR ys ussegd)


  -- Pack and Combine ---------------------------
  {-# INLINE_PDATA packByTagPR #-}
  packByTagPR (PTuple2 arr1 arr2) tags tag
        = PTuple2 (packByTagPR arr1 tags tag)
                  (packByTagPR arr2 tags tag)


  {-# INLINE_PDATA combine2PR #-}
  combine2PR sel (PTuple2 xs1 ys1) (PTuple2 xs2 ys2)
        = PTuple2 (combine2PR sel xs1 xs2)
                  (combine2PR sel ys1 ys2)


  -- Conversions --------------------------------
  {-# INLINE_PDATA fromVectorPR #-}
  fromVectorPR vec
   = let (xs, ys)       = V.unzip vec
     in  PTuple2 (fromVectorPR xs)
                 (fromVectorPR ys)


  {-# INLINE_PDATA toVectorPR #-}
  toVectorPR (PTuple2 xs ys)
        = V.zip  (toVectorPR xs)
                 (toVectorPR ys)


  -- PData --------------------------------------
  {-# INLINE_PDATA emptydPR #-}
  emptydPR      
        = PTuple2s emptydPR emptydPR

  
  {-# INLINE_PDATA singletondPR #-}
  singletondPR (PTuple2 x y)
        = PTuple2s (singletondPR x) (singletondPR y)


  {-# INLINE_PDATA lengthdPR #-}
  lengthdPR (PTuple2s xs ys)
        = lengthdPR xs

   
  {-# INLINE_PDATA indexdPR #-}
  indexdPR (PTuple2s xs ys) i
        = PTuple2 (indexdPR xs i) (indexdPR ys i)

   
  {-# INLINE_PDATA appenddPR #-}
  appenddPR (PTuple2s xs1 ys1) (PTuple2s xs2 ys2)
        = PTuple2s (appenddPR xs1 xs2) (appenddPR ys1 ys2)
  

  {-# INLINE_PDATA concatdPR #-}
  concatdPR vecs
        = PTuple2s
                (concatdPR $ V.map (\(PTuple2s xs ys) -> xs) vecs)
                (concatdPR $ V.map (\(PTuple2s xs ys) -> ys) vecs)


  {-# INLINE_PDATA fromVectordPR #-}
  fromVectordPR vec
   = let (xss, yss) = V.unzip $ V.map (\(PTuple2 xs ys) -> (xs, ys)) vec
     in  PTuple2s (fromVectordPR xss) (fromVectordPR yss)


  {-# INLINE_PDATA toVectordPR #-}
  toVectordPR (PTuple2s pdatas1 pdatas2)
        = V.zipWith PTuple2
                (toVectordPR pdatas1)
                (toVectordPR pdatas2)


-- PD Functions ---------------------------------------------------------------
-- These work on PData arrays of tuples, but don't need a PA or PR dictionary

-- | O(1). Zip a pair of arrays into an array of pairs.
zipPD   :: PData a -> PData b -> PData (a, b)
zipPD   = PTuple2
{-# INLINE_PA zipPD #-}


-- | Lifted zip.
ziplPD   :: PData (PArray a) -> PData (PArray b) -> PData (PArray (a, b))
ziplPD (PNested vsegd pdatas1) (PNested _ pdatas2)
        = PNested vsegd (PTuple2s pdatas1 pdatas2)
{-# INLINE_PA ziplPD #-}


-- | O(1). Unzip an array of pairs into a pair of arrays.
unzipPD :: PData (a, b) -> (PData a, PData b)
unzipPD (PTuple2 xs ys) = (xs, ys)
{-# INLINE_PA unzipPD #-}


-- | Lifted unzip.
unziplPD  :: PData (PArray (a, b)) -> PData (PArray a, PArray b)
unziplPD (PNested uvsegd (PTuple2s xsdata ysdata))
 =      PTuple2 (PNested uvsegd xsdata)
                (PNested uvsegd ysdata)
{-# INLINE_PA unziplPD #-}


-- PArray functions -----------------------------------------------------------
-- These work on PArrays of tuples, but don't need a PA or PR dictionary.

-- | O(1). Zip a pair of arrays into an array of pairs.
--   The two arrays must have the same length, else `error`. 
zip :: PArray a -> PArray b -> PArray (a, b)
zip (PArray n# pdata1) (PArray _ pdata2)
        = PArray n# $ zipPD pdata1 pdata2
{-# INLINE_PA zip #-}


-- | Lifted zip.
zipl :: PArray (PArray a) -> PArray (PArray b) -> PArray (PArray (a, b))
zipl (PArray n# xs) (PArray _ ys)
        = PArray n# $ ziplPD xs ys


-- | O(1). Unzip an array of pairs into a pair of arrays.
unzip :: PArray (a, b) -> (PArray a, PArray b)
unzip (PArray n# (PTuple2 xs ys))
        = (PArray n# xs, PArray n# ys)
{-# INLINE_PA unzip #-}


-- | Lifted unzip
unzipl :: PArray (PArray (a, b)) -> PArray (PArray a, PArray b)
unzipl (PArray n# pdata)
        = PArray n# $ unziplPD pdata


-- Show -----------------------------------------------------------------------
deriving instance (Show (PData  a), Show (PData  b)) => Show (PData  (a, b))
deriving instance (Show (PDatas a), Show (PDatas b)) => Show (PDatas (a, b))


instance ( PR a, PR b, Show a, Show b
         , PprVirtual (PData a), PprVirtual (PData b))
        => PprVirtual (PData (a, b)) where
 pprv   (PTuple2 xs ys)
        = text $ show 
        $ P.zip (V.toList $ toVectorPR xs) 
                (V.toList $ toVectorPR ys)

