{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators,TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module LArray where

import qualified Array as A
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))
import Control.Exception
import Debug.Trace 

data LArray dim e where 
  LArray :: dim -> (dim -> e) -> LArray dim e

--  Basic structural operations
--  ===========================

toLArray:: (U.Elt e, A.Shape dim) => A.Array dim e -> LArray dim e
{-# INLINE toLArray #-}
toLArray arr = LArray (A.arrayShape arr) (\i -> ((A.arrayData arr) U.!: (A.index (A.arrayShape arr) i)))

fromLArray:: (U.Elt e, A.Shape dim) => LArray dim e -> A.Array dim e
{-# INLINE fromLArray #-}
fromLArray (LArray shape fn)
   = A.Array { A.arrayData = U.map fn (A.range shape)
             , A.arrayShape = shape}

backpermute:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  LArray dim e -> dim' -> (dim' -> dim) -> LArray dim' e
{-# INLINE backpermute #-}
backpermute (LArray shape fn) newSh fn' =
  LArray newSh (fn.fn') 

backpermuteDft::(U.Elt e, A.Shape dim, A.Shape dim') => 
  LArray dim e -> e -> dim' -> (dim' -> Maybe dim) -> LArray dim' e
{-# INLINE backpermuteDft #-}
backpermuteDft srcArr@(LArray sh fn) e newSh fn' = 
  LArray newSh fn''
  where
    fn'' i = case (fn' i) of
               Just i' -> fn i'
               Nothing -> e  

--  Computations
--  ============

map:: (U.Elt a, U.Elt b, A.Shape dim) => (a -> b) -> LArray dim a -> LArray dim b
{-# INLINE map #-}
map fn' (LArray shape fn) = 
  LArray shape (fn'.fn)


zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> LArray dim a -> LArray dim b-> LArray dim c
{-# INLINE zipWith #-}
zipWith f (LArray shape1 fn1) (LArray shape2 fn2) = -- assert (shape1 == shape2) $
  LArray shape1 (\i -> f (fn1 i) (fn2 i))

{-
-- folds the innermost dimension - needs to be generalised
-- (has no advantage over using Array.mapFold) 
mapFold:: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> LArray (dim :*: Int) e  -> LArray dim  e
--{-# INLINE mapFold #-}
mapFold f n arr@(LArray sh@(sh' :*: s) fn) = 
  LArray sh' f'
  where
    f' i = U.fold f n (U.map (\(():*:s)-> fn (i:*:s)) (A.range (():*:s)))

-}
mapFold:: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> LArray (dim :*: Int) e  -> LArray dim  e
{-# INLINE mapFold #-}
mapFold f n arr@(LArray sh@(sh' :*: s) fn) = toLArray $
  A.Array{ A.arrayShape = sh'
         , A.arrayData  = U.fold_s f n  (U.lengthsToSegd $ U.replicate noOfSegs s) 
           (U.map fn (A.range sh))}
  where
    noOfSegs = (A.size sh) `div` s


    

----  Non-primitive functions 
----

shift:: (A.Shape dim, U.Elt e) => LArray dim e -> e -> dim -> LArray dim e
--{-# INLINE shift #-}
shift arr@(LArray sh _) e shiftOffset = backpermuteDft arr  e sh
  (\d -> if (A.inRange sh (A.addDim d shiftOffset)) then Just (A.addDim d shiftOffset) else Nothing)

rotate:: (A.Shape dim, U.Elt e) => LArray dim e -> e -> dim -> LArray dim e
{-# INLINE rotate #-}
rotate arr@(LArray sh _) e shiftOffset = backpermute arr  sh
  (\d -> A.addModDim sh d shiftOffset)



tile::  (A.Shape dim, U.Elt e) => LArray dim e -> dim -> dim -> LArray dim e
{-# INLINE tile #-}
tile arr@(LArray sh _) start size = 
--  assert (A.inRange sh (A.addDim start size)) $
     backpermute arr size 
     (\d -> A.addDim d start)


--  Combining arrays
-- 

--   
--
append:: (A.Shape dim, U.Elt e) => LArray dim e -> LArray dim e -> dim -> LArray dim e
{-# INLINE append #-}
append arr1@(LArray sh1 fn1) arr2@(LArray sh2 fn2) newSh =
  LArray newSh appFn
  where
    appFn i = if (A.inRange sh1 i) 
                then fn1 i
                else fn2 (A.modDim i sh1)
  

--  Shape polymorphic ops based on Index
--  ====================================


select:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  LArray dim e -> A.SelectIndex dim dim'  -> LArray dim' e
{-# INLINE select #-}
select arr@(LArray shape _ ) ind = 
  backpermute arr (A.projShape ind shape) (selectFun ind)
  where
    selectFun:: A.SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun A.IndexNil () = ()
    selectFun (A.IndexAll rsh) (shs :*: s) = (selectFun rsh shs) :*: s
    selectFun (A.IndexFixed n rsh) shs     = (selectFun rsh shs) :*: n

replicate:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  LArray dim' e -> A.SelectIndex dim dim'  -> LArray dim e
{-# INLINE replicate #-}
replicate arr@(LArray shape _ ) ind = 
  backpermute arr (A.initShape ind shape) (repFun ind)
  where
    repFun:: A.SelectIndex dim1 dim2 -> dim1 -> dim2
    repFun A.IndexNil () = ()
    repFun (A.IndexAll rsh) (shs :*: s) = (repFun rsh shs) :*: s
    repFun (A.IndexFixed _ rsh) (shs :*: _) = repFun rsh shs


index::(U.Elt e, A.Shape dim) => LArray dim e -> dim -> e
{-# INLINE index #-}
index arr@(LArray _ fn) i =
    fn i  


-- uses the 'standard' library functions
transpose:: U.Elt e => LArray A.DIM2 e -> LArray A.DIM2 e
{-# INLINE transpose #-}
transpose arr@(LArray (() :*:n :*: m) fn) = 
  backpermute arr  (() :*: m :*: n) (\((() :*: i) :*: j) -> ((() :*: j) :*: i))


mmMult:: LArray A.DIM2 Double -> LArray A.DIM2 Double -> LArray A.DIM2 Double
mmMult arr1@(LArray (() :*: m1 :*: n1) fn1) arr2@(LArray (() :*: m2 :*: n2) fn2) = assert (m1 == n2) $ 
  mapFold (+) 0 $ LArray.zipWith (*) arr1Ext arr2Ext
  where
    arr1Ext = LArray.replicate arr1 (A.IndexAll (A.IndexFixed m2 (A.IndexAll A.IndexNil)))
    arr2Ext = LArray.replicate 
                (transpose arr2) (A.IndexAll (A.IndexAll (A.IndexFixed n1 A.IndexNil)))


relaxShift:: A.Array A.DIM2 Double -> A.Array A.DIM2 Double
{-# INLINE relaxShift #-}
relaxShift arr' =  fromLArray $
  LArray.map ( (/) 5) $ 
     LArray.zipWith (+) 
    (LArray.zipWith (+) (LArray.zipWith (+) shiftu arr) shiftl) (LArray.zipWith (+) shiftr shiftd)
  where
    arr = toLArray arr'
    s@((() :*: n) :*: m) = A.arrayShape arr'
    shiftu = shift arr 0 (():*: 1   :*:0)
    shiftd = shift arr 0 (():*:(-1) :*:0)
    shiftl = shift arr 0 (():*: 0   :*:1)
    shiftr = shift arr 0 (():*: 0   :*:(-1))

