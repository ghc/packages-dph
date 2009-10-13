{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DArray ( 
    DArray (..)
  , toDArray 
  , fromDArray
  , forceDArray
  , backpermute
  , backpermuteDft
  , map
  , zip
  , zipWith
  , fold
  , mapFold
  , mapStencil
  , shift
  , reshape
  , rotate
  , tile
  , append
  , select
  , replicate
  , index
  , splitDArray
  , joinDArray
  ) where
         
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))


import qualified Array as A
import Prelude hiding (map, zip, zipWith, replicate)



data DArray dim e where 
  DArray :: dim -> (dim -> e) -> DArray dim e


assert a b = b


--  Basic structural operations
--  ===========================

-- |Convert a strict array into a delayed array
toDArray:: (U.Elt e, A.Shape dim) => A.Array dim e -> DArray dim e
{-# INLINE toDArray #-}
toDArray arr = 
  DArray (A.arrayShape arr) 
         (\i -> ((A.arrayData arr) U.!: (A.index (A.arrayShape arr) i)))


-- |Convert delayed array into strict array, force evaluation
fromDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> A.Array dim e
{-# INLINE fromDArray #-}
fromDArray (DArray shape fn)
   = A.Array { A.arrayData = 
                     -- this is faster at the moment
                     U.map (fn . i) (U.enumFromTo (0::Int) ((A.size shape) - 1))  
                     -- U.map fn (A.range shape)
             , A.arrayShape = shape}
     where
       i = A.indexInv shape

forceDArray:: (U.Elt e, A.Shape dim) => DArray dim e -> DArray dim e
{-# INLINE forceDArray #-}
forceDArray = toDArray . fromDArray

fromDArray2:: (U.Elt e) => DArray A.DIM2 e -> A.Array A.DIM2 e
{-# INLINE fromDArray2 #-}
fromDArray2 (DArray shape fn)
   = A.Array { A.arrayData = 
                     U.map fn (range2 shape)
             , A.arrayShape = shape}
   where
     range2:: A.DIM2 -> U.Array A.DIM2
     range2  ((():*: n) :*:  m) = U.zipWith (:*:) 
                 (U.zipWith (:*:)
                    (U.replicate (nm) ()) 
                    (U.map (`mod` m)  (U.enumFromTo 0 (nm-1))))
                 (U.map (`div` n) (U.enumFromTo 0 (nm-1)))
        where
          nm = n*m


-- |Generalised array backpermutation: arguments: delayed array, a target shape
--  and a function mapping each index in the target shape range to an index 
--  of the src array range.
backpermute:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> dim' -> (dim' -> dim) -> DArray dim' e
{-# INLINE backpermute #-}
backpermute (DArray shape fn) newSh fn' =
  DArray newSh (fn.fn') 

backpermuteDft::(U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> e -> dim' -> (dim' -> Maybe dim) -> DArray dim' e
{-# INLINE backpermuteDft #-}
backpermuteDft srcArr@(DArray sh fn) e newSh fn' = 
  DArray newSh fn''
  where
    fn'' i = case (fn' i) of
               Just i' -> fn i'
               Nothing -> e  

--  Computations
--  ============

-- | Map function over each element of N-dim DArray
map:: (U.Elt a, U.Elt b, A.Shape dim) => (a -> b) -> DArray dim a -> DArray dim b
{-# INLINE map #-}
map fn' (DArray shape fn) = 
  DArray shape (fn'.fn)

-- | zipWith assumes both src arrays to be of the same shape
zipWith:: (U.Elt a, U.Elt b, U.Elt c, A.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
{-# INLINE zipWith #-}
zipWith f (DArray shape1 fn1) (DArray shape2 fn2) = -- assert (shape1 == shape2) $
  DArray shape1 (\i -> f (fn1 i) (fn2 i))


-- | zipWith assumes both src arrays to be of the same shape
zip:: (U.Elt a, U.Elt b, A.Shape dim) => 
  DArray dim a -> DArray dim b-> DArray dim (a :*: b)
{-# INLINE zip #-}
zip (DArray shape1 fn1) (DArray shape2 fn2) = -- assert (shape1 == shape2) $
  DArray shape1 (\i -> (fn1 i) :*: (fn2 i))
         
fold :: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> DArray dim e  -> e
{-# INLINE fold #-}
fold f n arr = 
  A.fold f n  $ fromDArray arr


-- folds the innermost dimension - needs to be generalised
mapFold:: (U.Elt e, A.Shape dim) => (e -> e-> e) -> e -> DArray (dim :*: Int) e  -> DArray dim  e
{-# INLINE mapFold #-}
mapFold f n arr@(DArray sh@(sh' :*: s) fn) = 
  DArray sh' f'
  where
    f' i = U.fold f n (U.map (\(():*:s)-> fn (i:*:s)) (A.range (():*:s)))


mapStencil:: (A.Shape dim, A.Shape dim', U.Elt e) =>
  (dim -> Bool) -> dim' -> (dim -> dim' -> dim) -> (e -> e') -> (DArray dim' e -> e') -> DArray dim e -> DArray dim e'
mapStencil border stencilSize stencil g f arr@(DArray sh arrFn) =
  DArray sh resFn
  where
    resFn d = 
      if (border d)
        then g $ arrFn d
        else let df' = \d' -> arrFn (stencil d  d')
             in f (DArray stencilSize df')           


----  Non-primitive functions 
----

shift:: (A.Shape dim, U.Elt e) => DArray dim e -> e -> dim -> DArray dim e
{-# INLINE shift #-}
shift arr@(DArray sh _) e shiftOffset = backpermuteDft arr  e sh
  (\d -> if (A.inRange sh (A.addDim d shiftOffset)) 
           then Just (A.addDim d shiftOffset) 
           else Nothing)

reshape:: (A.Shape dim, A.Shape dim', U.Elt e) => DArray dim e -> dim' -> DArray dim' e
reshape arr@(DArray sh fn) newShape = assert (A.size newShape == A.size sh) $
  DArray newShape (fn .  (A.indexInv sh). (A.index newShape))






rotate:: (A.Shape dim, U.Elt e) => DArray dim e -> e -> dim -> DArray dim e
{-# INLINE rotate #-}
rotate arr@(DArray sh _) e shiftOffset = backpermute arr  sh
  (\d -> A.addModDim sh d shiftOffset)



tile::  (A.Shape dim, U.Elt e) => DArray dim e -> dim -> dim -> DArray dim e
{-# INLINE tile #-}
tile arr@(DArray sh _) start size = 
--  assert (A.inRange sh (A.addDim start size)) $
     backpermute arr size 
     (\d -> A.addDim d start)


--  Combining arrays
-- 

--   
--
append:: (A.Shape dim, U.Elt e) => DArray dim e -> DArray dim e -> dim -> DArray dim e
{-# INLINE append #-}
append arr1@(DArray sh1 fn1) arr2@(DArray sh2 fn2) newSh =
  DArray newSh appFn
  where
    appFn i = if (A.inRange sh1 i) 
                then fn1 i
                else fn2 (A.modDim i sh1)
  

--  Shape polymorphic ops based on Index
--  ====================================


select:: (U.Elt e, A.Shape dim, A.Shape dim') => 
  DArray dim e -> A.SelectIndex dim dim'  -> DArray dim' e
{-# INLINE select #-}
select arr@(DArray shape _ ) ind = 
  backpermute arr (A.projShape ind shape) (selectFun ind)
  where
    selectFun:: A.SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun A.IndexNil () = ()
    selectFun (A.IndexAll rsh) (shs :*: s) = (selectFun rsh shs) :*: s
    selectFun (A.IndexFixed n rsh) shs     = (selectFun rsh shs) :*: n




replicate:: (U.Elt e, A.Shape dim, A.Shape dim', A.InitShape dim, A.RepFun dim) => 
  DArray dim' e -> A.SelectIndex dim dim'  -> DArray dim e
{-# INLINE replicate #-}
replicate arr@(DArray shape _ ) ind = 
  backpermute arr (A.initShape ind shape) (A.repFun ind)



index::(U.Elt e, A.Shape dim) => DArray dim e -> dim -> e
{-# INLINE index #-}
index arr@(DArray _ fn) i =
    fn i  


--  Split and Joins should result in irregular arrays with regular element type. For now, though, 
--  it's implemented using lists

splitDArray:: (U.Elt e, A.Shape dim) => 
  DArray dim e -> A.MapIndex dim dim' ->  [DArray dim' e]
splitDArray _ =
  error "splitDArray: not yet implemented"

joinDArray:: (U.Elt e, A.Shape dim) => [DArray dim e] -> DArray (dim :*: Int) e 
joinDArray _ = 
  error "joinDArray: not yet implemented"