{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Lifted.Selector
where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Base ( fromBool, (:*:)(..) )

import GHC.Exts (Int#, Int(..))

data Sel2 = Sel2 (U.Array Int) (U.Array Int) !Int !Int

mkSel2 :: U.Array Int -> U.Array Int -> Int -> Int -> Sel2
{-# INLINE CONLIKE PHASE_BACKEND mkSel2 #-}
mkSel2 = Sel2

tagsToIndices2 :: U.Array Int -> U.Array Int
{-# INLINE_BACKEND tagsToIndices2 #-}
tagsToIndices2 ts = U.zipWith pick ts
                  . U.scan idx (0 :*: 0)
                  $ U.map start ts
  where
    start 0 = 1 :*: 0
    start _ = 0 :*: 1

    idx (i1 :*: j1) (i2 :*: j2) = (i1+i2 :*: j1+j2)

    pick 0 (i :*: j) = i
    pick _ (i :*: j) = j

tagsToSel2 :: U.Array Int -> Sel2
{-# INLINE tagsToSel2 #-}
tagsToSel2 ts = mkSel2 ts (tagsToIndices2 ts) (U.count ts 0) (U.count ts 1)

tagsSel2 :: Sel2 -> U.Array Int
{-# INLINE_BACKEND tagsSel2 #-}
tagsSel2 (Sel2 tags idxs na nb) = tags

indicesSel2 :: Sel2 -> U.Array Int
{-# INLINE_BACKEND indicesSel2 #-}
indicesSel2 (Sel2 tags idxs na nb) = idxs

elementsSel2_0 :: Sel2 -> Int
{-# INLINE_BACKEND elementsSel2_0 #-}
elementsSel2_0 (Sel2 tags idxs na nb) = na

elementsSel2_0# :: Sel2 -> Int#
{-# INLINE elementsSel2_0# #-}
elementsSel2_0# (Sel2 _ _ (I# na#) _) = na#

elementsSel2_1 :: Sel2 -> Int
{-# INLINE_BACKEND elementsSel2_1 #-}
elementsSel2_1 (Sel2 tags idxs na nb) = nb

elementsSel2_1# :: Sel2 -> Int#
{-# INLINE elementsSel2_1# #-}
elementsSel2_1# (Sel2 _ _ _ (I# nb#)) = nb#

empty2 :: Sel2
{-# INLINE empty2 #-}
empty2 = mkSel2 U.empty U.empty 0 0

replicate2 :: Int -> Int -> Sel2
{-# INLINE replicate2 #-}
replicate2 n tag = mkSel2 (U.replicate n tag)
                          (U.enumFromTo 0 (n-1))
                          (if n == 0 then n else 0)
                          (if n == 0 then 0 else n)

pick2 :: Sel2 -> Int# -> U.Array Bool
{-# INLINE pick2 #-}
pick2 sel tag# = U.pick (tagsSel2 sel) (I# tag#)
                              
