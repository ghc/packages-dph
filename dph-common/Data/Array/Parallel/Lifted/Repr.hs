{-# LANGUAGE EmptyDataDecls, TemplateHaskell #-}
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Repr (
  PData(..),
  Void, void, pvoid, fromVoid,

  punit,
  Wrap(..),

  zipPA#, unzipPA#, zip3PA#,
  Sum2(..), {-Sum3(..),-} {-dPR_Sum3,-}

  segdPA#, concatPA#, segmentPA#, copySegdPA#
) where
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PDataInstances
import Data.Array.Parallel.Lifted.TH.Repr
import Data.Array.Parallel.Lifted.PArray
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd#, elementsSel2_0#,
                                            elementsSel2_1# )

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base (intToTag, fromBool)
import Data.Array.Parallel.Base.DTrace ( traceFn, traceArg )

import Data.List (unzip4, unzip5)
import GHC.Exts  (Int#, Int(..), (+#), (-#), (*#))
import GHC.Word  ( Word8 )


-- Sums -----------------------------------------------------------------------
zipPA# :: PArray a -> PArray b -> PArray (a,b)
{-# INLINE_PA zipPA# #-}
zipPA# (PArray n# xs) (PArray _ ys) = PArray n# (P_2 xs ys)

unzipPA# :: PArray (a,b) -> (PArray a, PArray b)
{-# INLINE_PA unzipPA# #-}
unzipPA# (PArray n# (P_2 xs ys)) = (PArray n# xs, PArray n# ys)


zip3PA# :: PArray a -> PArray b -> PArray c -> PArray (a,b,c)
{-# INLINE_PA zip3PA# #-}
zip3PA# (PArray n# xs) (PArray _ ys) (PArray _ zs) = PArray n# (P_3 xs ys zs)


-- Nested Arrays --------------------------------------------------------------
segdPA# :: PArray (PArray a) -> U.Segd
{-# INLINE_PA segdPA# #-}
segdPA# (PArray _ (PNested segd _)) = segd

concatPA# :: PArray (PArray a) -> PArray a
{-# INLINE_PA concatPA# #-}
concatPA# (PArray _ (PNested segd xs)) = PArray (elementsSegd# segd) xs

segmentPA# :: Int# -> U.Segd -> PArray a -> PArray (PArray a)
{-# INLINE_PA segmentPA# #-}
segmentPA# n# segd (PArray _ xs) = PArray n# (PNested segd xs)

copySegdPA# :: PArray (PArray a) -> PArray b -> PArray (PArray b)
{-# INLINE copySegdPA# #-}
copySegdPA# (PArray n# (PNested segd _)) (PArray _ xs)
  = PArray n# (PNested segd xs)

