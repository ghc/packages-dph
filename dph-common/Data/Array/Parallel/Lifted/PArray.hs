{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

-- |Underlying data types and type classes for parallel arrays.
--
--	For motivational material see:
--	   "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--	For discussion of how the mapping to generic types works see:
--         "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packByTagPA#, combine2PA#, updatePA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packByTagPD, combine2PD, updatePD, fromListPD, nfPD,

  PRepr, PR(..),

  Scalar(..),
  replicatePRScalar, replicatelPRScalar, repeatPRScalar, emptyPRScalar,
  indexPRScalar, extractPRScalar, bpermutePRScalar, appPRScalar, applPRScalar,
  packByTagPRScalar, combine2PRScalar, updatePRScalar, fromListPRScalar,
  nfPRScalar,
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import Data.Array.Parallel.Base           ( Tag, intToTag, traceF )
import Data.Array.Parallel.PArray.PRepr
import Data.Array.Parallel.PArray.Scalar
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.Base
import GHC.Exts (Int#, Int(..), (+#), (*#))
import SpecConstr




