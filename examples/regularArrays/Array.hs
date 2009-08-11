{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances, TypeOperators,TypeSynonymInstances  #-}
--
-- |N-dimensional array library
--
--  Copyright (c) [2009] Gabriele Keller
--
--  License: BSD3
--
--- Description ---------------------------------------------------------------
--
--
--  
--
--
module Array where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))
import Control.Exception


import Debug.Trace

instance U.Elt ()

-- |Arrays
-- -------
data Array dim e where
  Array { arrayShape    :: dim                -- ^extend of dimensions
        , arrayData     :: U.Array e          -- flat parallel array
        }               :: Array dim e
  deriving Show

-- |Shorthand for various dimensions
--
type DIM0 = ()
type DIM1 = (DIM0 :*: Int)
type DIM2 = (DIM1 :*: Int)
type DIM3 = (DIM2 :*: Int)
type DIM4 = (DIM3 :*: Int)
type DIM5 = (DIM4 :*: Int)

data Index a initialDim projectedDim where
  IndexNil   :: Index a () ()
  IndexAll   :: (Shape init, Shape proj) =>      
                   Index a init proj -> Index a (init :*: Int) (proj :*: Int)
  IndexFixed :: (Shape init, Shape proj) => a -> 
                   Index a init proj -> Index a (init :*: Int)  proj



type SelectIndex = Index Int
type MapIndex    = Index ()

type family (:-:) init proj
type instance (:-:) init () = init
type instance (:-:) (init :*: Int) (proj :*: Int) = ((:-:) init proj) 


-- |Our index class
--
class (Show sh, U.Elt sh) => Shape sh where
  dim   :: sh -> Int           -- ^number of dimensions (>= 0)
  size  :: sh -> Int           -- ^for a *shape* yield the total number of 
                               -- elements in that array
  index :: sh -> sh -> Int     -- ^corresponding index into a linear, row-major 
                               -- representation of the array (first argument
                               -- is the shape)
  indexInv:: sh -> Int -> sh   -- ^given index into linear row major representation,
                               -- calculates index into array
  range      :: sh -> U.Array sh  -- all the valid indices in a shape. The following
                                  -- equality should hold: 
                                  -- map (index sh) (range sh) = [:0..(size sh)-1:]
  inRange    :: sh -> sh -> Bool
  zeroDim    :: sh
  addDim     :: sh -> sh -> sh    
  modDim     :: sh -> sh -> sh    
  addModDim  :: sh -> sh -> sh -> sh

  last    :: (sh :*: Int) -> Int
  inits   :: (sh :*: Int) -> sh
  
instance Shape () where
  dim n  = 0
  size n = 1
  index sh n    = 0
  indexInv sh _ = ()

  {-# INLINE range #-}
  range sh   = U.fromList [()]
  inRange () () = True
  zeroDim = ()
--  {-# INLINE addDim #-}
  addDim _ _  = ()
  modDim _ _  = ()
  addModDim _ _ _ = ()
  last (() :*: n) = n
  inits (() :*: n) = ()

instance Shape sh => Shape (sh :*: Int) where
  dim   (sh  :*: _)                 = dim sh + 1
  size  (sh1 :*: n)                 = size sh1 * n
  index (sh1 :*: sh2) (sh1' :*: sh2') = index sh1 sh1' * sh2 + sh2'
  indexInv (ds :*: d) n = 
    let r = n `div` d
        i = n `mod` d
    in (indexInv ds r) :*: i

      

-- adding this inline pragma make hmmult very slow
--  {-# INLINE addDim #-}
  addDim (sh1 :*: n1) (sh2 :*: n2) = ((addDim sh1 sh2) :*: (n1 + n2))
  addModDim (aSh :*: a) (bSh :*: b) (cSh :*: m) =
    (addModDim aSh bSh cSh :*: ((a + b + 1) `mod` m) -1)


  modDim (sh1 :*: n1) (sh2 :*: n2) = (modDim sh1 sh2 :*: (n1 `mod` n2))
  {-# INLINE range #-}
  range (sh :*: n) = U.zipWith (\r -> \t -> (r :*: t)) 
    (U.replicate_s (U.lengthsToSegd  $ U.replicate (U.length rsh) n) rsh)
    (U.repeat (U.length rsh) (n*n) (U.enumFromTo 0 (n-1)))
    where rsh = range sh
  inRange (sh1 :*: n1) (sh2 :*: n2) = (n2 >= 0) && (n2 < n1) && (inRange sh1 sh2)
  zeroDim = (zeroDim :*: 0)
  last  (sh :*: n) = Array.last sh
  inits (sh :*: n) = (Array.inits sh) :*: n


  
--  Basic structural operations
--  ===========================

toArray:: (U.Elt e, Shape dim) => dim -> U.Array e -> Array dim e
--{-# INLINE toArray #-}
toArray dim arr = assert (size dim == U.length arr) $
  Array dim arr

fromArray:: (U.Elt e, Shape dim) => Array dim e -> U.Array e 
--{-# INLINE fromArray #-}
fromArray = arrayData


backpermute:: (U.Elt e, Shape dim, Shape dim') => 
  Array dim e -> dim' -> (dim' -> dim) -> Array dim' e
{-# INLINE backpermute #-}
backpermute arr newSh fn = 
  Array newSh $ U.bpermute (arrayData arr) $ (U.map (index $ arrayShape arr)) $ U.map fn $ range newSh

backpermuteDft::(U.Elt e, Shape dim, Shape dim') => 
  Array dim e -> e -> dim' -> (dim' -> Maybe dim) -> Array dim' e
--{-# INLINE backpermuteDft #-}
backpermuteDft srcArr e newSh fn = 
  Array newSh $ U.bpermuteDft (size newSh) init inds 
  where
    sh = arrayShape srcArr
    init = const e
    inds = (uncurry U.zip) $ (\(di :*: si) -> (di,  U.bpermute (arrayData srcArr) si)) $  
                             U.unzip $ 
                             U.filter (\(dstInd :*: srcInd) -> srcInd > -1) $ 
                             U.map fn' $ range newSh

    fn' d = case fn d of
              Just a  -> (index newSh d) :*: (index sh a)
              Nothing -> (index newSh d) :*: (-1) 

--  Shape polymorphic ops based on Index
--  ====================================


select:: (U.Elt e, Shape dim, Shape dim') => Array dim e ->SelectIndex dim dim'  -> Array dim' e
--{-# INLINE select #-}
select arr ind = 
  backpermute arr (projShape ind (arrayShape arr)) (selectFun ind)
  where
    selectFun:: SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun IndexNil () = ()
    selectFun (IndexAll rsh) (shs :*: s) = (selectFun rsh shs) :*: s
    selectFun (IndexFixed n rsh) shs     = (selectFun rsh shs) :*: n

replicate:: (U.Elt e, Shape dim, Shape dim') => Array dim' e ->SelectIndex dim dim'  -> Array dim e
--{-# INLINE replicate #-}
replicate arr ind = -- trace (show $ (initShape ind (arrayShape arr))) $
  backpermute arr (initShape ind (arrayShape arr)) (repFun ind)
  where
    repFun:: SelectIndex dim1 dim2 -> dim1 -> dim2
    repFun IndexNil () = ()
    repFun (IndexAll rsh) (shs :*: s) = (repFun rsh shs) :*: s
    repFun (IndexFixed _ rsh) (shs :*: _) = repFun rsh shs



-- Given a selector index and the initial dimension, calculate the dim of
-- the resulting projection 
projShape:: (Shape dim, Shape dim') => SelectIndex dim dim' -> dim -> dim'
projShape IndexNil () = ()
projShape (IndexAll ixs)     (shs :*: s) = (projShape ixs shs) :*: s
projShape (IndexFixed _ ixs) (shs   :*: s) = projShape ixs shs

-- Given a replicator index and the initial dimension, calculate the dim of
-- the resulting replication
initShape:: (Shape dim, Shape dim') => SelectIndex dim dim' -> dim' -> dim
initShape IndexNil () = ()
initShape (IndexFixed n rsh) shs       = (initShape rsh shs) :*: n
initShape (IndexAll rsh) (shs :*: s)   = (initShape rsh shs) :*: s



-- Computations
-- ============
map:: (U.Elt a, U.Elt b, Shape dim) => (a -> b) -> Array dim a -> Array dim b
{-# INLINE map #-}
map f arr = arr{arrayData = U.map f $ arrayData arr}


zipWith:: (U.Elt a, U.Elt b, U.Elt c, Shape dim) => 
  (a -> b -> c) -> Array dim a -> Array dim b-> Array dim c
{-# INLINE zipWith #-}
zipWith f arr1 arr2 = arr1{arrayData = U.zipWith f (arrayData arr1) (arrayData arr2)}

-- folds the innermost dimension - needs to be generalised 
mapFold:: (U.Elt e, Shape dim) => (e -> e-> e) -> e -> Array (dim :*: Int) e  -> Array dim  e
{-# INLINE mapFold #-}
mapFold f n arr = 
  Array{ arrayShape = inits (arrayShape arr)
       , arrayData  = U.fold_s f n  (U.lengthsToSegd $ U.replicate noOfSegs segSize) (arrayData arr)}
  where
    segSize = Array.last $ arrayShape arr
    noOfSegs = (U.length $ arrayData arr) `div` segSize


zip:: (U.Elt a, U.Elt b, Shape dim) => Array dim a -> Array dim b-> Array dim (a :*: b)
{-# INLINE zip #-}
zip arr1 arr2 = arr1{arrayData = U.zip (arrayData arr1) (arrayData arr2)}

----  Non-primitive functions - need to be moved to different module
----


reshape:: (Shape dim', Shape dim, U.Elt e) => Array dim e -> dim' -> Array dim' e
reshape arr newShape = assert (size newShape == size (arrayShape arr)) $
  arr{arrayShape = newShape}

shift:: (Shape dim, U.Elt e) => Array dim e -> e -> dim -> Array dim e
{-# INLINE shift #-}
shift arr e shiftOffset = backpermuteDft arr  e sh
  (\d -> if (inRange sh (addDim d shiftOffset)) then Just (addDim d shiftOffset) else Nothing)
  where
    sh = arrayShape arr

rotate:: (Shape dim, U.Elt e) => Array dim e -> e -> dim -> Array dim e
rotate arr e shiftOffset = backpermute arr  sh
  (\d -> addModDim sh d shiftOffset)
  where
    sh = arrayShape arr


tile::  (Shape dim, U.Elt e) => Array dim e -> dim -> dim -> Array dim e
tile arr start size = 
  assert (inRange (arrayShape arr) (addDim start size)) $
     backpermute arr size 
     (\d -> addDim d start)


--  Alternative implementations for lib functions
--  ---------------------------------------------
--
myMap:: (U.Elt a, U.Elt b) => (a -> b) -> Array dim a -> Array dim b 
myMap f arr = arr{arrayData = U.mbpermute f arrd (U.enumFromTo 0 ((U.length arrd)-1))}
  where arrd = arrayData arr

myZipWith:: (U.Elt a, U.Elt b, U.Elt c) => (a -> b -> c) -> U.Array a -> U.Array b -> U.Array c
myZipWith f arr1 arr2 = 
 U.mbpermute f' (U.zip arr1 arr2) (U.enumFromTo 0 ((U.length arr1)-1))
 where 
   f' (a :*: b) = f a b


--  module ArrayExamples ...
--  =========================
--
--  some examples using strict arrays
--

-- Matrix transposition
------------------------

-- uses the 'standard' library functions
ae_transpose:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE ae_transpose #-}
ae_transpose arr = 
  backpermute arr (() :*:n :*: m) (\((() :*: i) :*: j) -> ((() :*: j) :*: i))
  where
    (() :*:n :*: m) = arrayShape arr


-- avoids index/tuple calculations
ae_transposePrim:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE ae_transposePrim #-}
ae_transposePrim arr = arr{arrayData = U.bpermute (arrayData arr) inds}
  where
   (_ :*: n) = arrayShape arr
   inds      = U.zipWith (+)
                 (U.repeat n (n*n) (U.enumFromStepLen 0 n n))
                 (U.replicate_s (U.lengthsToSegd  $ U.replicate n n) (U.enumFromTo 0 (n-1)))


-- uses default backpermute 
ae_transposeDFT:: (U.Elt a, Num a) => Array DIM2 a ->        Array DIM2 a
{-# INLINE ae_transposeDFT #-}
ae_transposeDFT arr = assert (n==m) $
  backpermuteDft arr 0 (arrayShape arr) (\((() :*: i) :*: j) -> Just ((() :*: j) :*: i))
  where
    (() :*:n :*: m) = arrayShape arr



-- Matrix relaxation
-- -----------------

ae_relax:: (U.Elt a, Fractional a) => Array DIM2 a -> Array DIM2 a
{-# INLINE ae_relax #-}
ae_relax arr =  
  Array.map ((/) 5) $ 
    Array.zipWith (+) 
    (Array.zipWith (+) (Array.zipWith (+) shiftu arr) shiftl) (Array.zipWith (+) shiftr shiftd)
  where
    s@((() :*: n) :*: m) = arrayShape arr
    shiftu = backpermuteDft arr 0 s  fu 
    shiftd = backpermuteDft arr 0 s  fd 
    shiftl = backpermuteDft arr 0 s  fl 
    shiftr = backpermuteDft arr 0 s  fr 
    fu = \((() :*: i) :*: j) -> if (i < (n-1)) then Just (() :*: (i+1) :*: j) else Nothing
    fd = \((() :*: i) :*: j) -> if (i > 0)     then Just (() :*: (i-1) :*: j) else Nothing
    fl = \((() :*: i) :*: j) -> if (j < (m-1)) then Just (() :*: i :*: (j+1)) else Nothing
    fr = \((() :*: i) :*: j) -> if (j > 0)     then Just (() :*: i :*: (j-1)) else Nothing

ae_relaxShift:: Array DIM2 Double -> Array DIM2 Double
{-# INLINE ae_relaxShift #-}
ae_relaxShift arr =  
  Array.map ( (/) 5) $ 
    Array.zipWith (+) 
    (Array.zipWith (+) (Array.zipWith (+) shiftu arr) shiftl) (Array.zipWith (+) shiftr shiftd)
  where
    s@((() :*: n) :*: m) = arrayShape arr
    shiftu = shift arr 0 (():*: 1   :*:0)
    shiftd = shift arr 0 (():*:(-1) :*:0)
    shiftl = shift arr 0 (():*: 0   :*:1)
    shiftr = shift arr 0 (():*: 0   :*:(-1))



-- Matrix Multiplication
-- ---------------------


ae_mmMult:: Array DIM2 Double -> Array DIM2 Double -> Array DIM2 Double
ae_mmMult arr1 arr2 = assert (m1 == n2) $ 
  mapFold (+) 0 $ Array.zipWith (*) arr1Ext arr2Ext
  where
    arr1Ext = Array.replicate arr1 (IndexAll (IndexFixed m2 (IndexAll IndexNil)))
    arr2Ext = Array.replicate (ae_transpose arr2) (IndexAll (IndexAll (IndexFixed n1 IndexNil)))
    (() :*: m1 :*: n1) = arrayShape arr1
    (() :*: m2 :*: n2) = arrayShape arr2


--
--  =======================
--  module DArray
--  =======================
--

data DArray dim e where 
  DArray :: dim -> (dim -> e) -> DArray dim e

--  Basic structural operations
--  ===========================

-- |Convert a strict array into a delayed array
toDArray:: (U.Elt e, Array.Shape dim) => Array.Array dim e -> DArray dim e
{-# INLINE toDArray #-}
toDArray arr = 
  DArray (Array.arrayShape arr) 
         (\i -> ((Array.arrayData arr) U.!: (Array.index (Array.arrayShape arr) i)))


-- |Convert delayed array into strict array, force evaluation
fromDArray:: (U.Elt e, Array.Shape dim) => DArray dim e -> Array.Array dim e
{-# INLINE fromDArray #-}
fromDArray (DArray shape fn)
   = Array.Array { Array.arrayData = U.map fn (Array.range shape)
             , Array.arrayShape = shape}


-- |Generalised array backpermutation: arguments: delayed array, a target shape
--  and a function mapping each index in the target shape range to an index 
--  of the src array range.
da_backpermute:: (U.Elt e, Array.Shape dim, Array.Shape dim') => 
  DArray dim e -> dim' -> (dim' -> dim) -> DArray dim' e
{-# INLINE da_backpermute #-}
da_backpermute (DArray shape fn) newSh fn' =
  DArray newSh (fn.fn') 

da_backpermuteDft::(U.Elt e, Array.Shape dim, Array.Shape dim') => 
  DArray dim e -> e -> dim' -> (dim' -> Maybe dim) -> DArray dim' e
{-# INLINE da_backpermuteDft #-}
da_backpermuteDft srcArr@(DArray sh fn) e newSh fn' = 
  DArray newSh fn''
  where
    fn'' i = case (fn' i) of
               Just i' -> fn i'
               Nothing -> e  

--  Computations
--  ============

-- | Map function over each element of N-dim DArray
da_map:: (U.Elt a, U.Elt b, Array.Shape dim) => (a -> b) -> DArray dim a -> DArray dim b
{-# INLINE da_map #-}
da_map fn' (DArray shape fn) = 
  DArray shape (fn'.fn)

-- | zipWith assumes both src arrays to be of the same shape
da_zipWith:: (U.Elt a, U.Elt b, U.Elt c, Array.Shape dim) => 
  (a -> b -> c) -> DArray dim a -> DArray dim b-> DArray dim c
{-# INLINE da_zipWith #-}
da_zipWith f (DArray shape1 fn1) (DArray shape2 fn2) = -- assert (shape1 == shape2) $
  DArray shape1 (\i -> f (fn1 i) (fn2 i))

-- folds the innermost dimension - needs to be generalised
da_mapFold:: (U.Elt e, Array.Shape dim) => (e -> e-> e) -> e -> DArray (dim :*: Int) e  -> DArray dim  e
{-# INLINE da_mapFold #-}
da_mapFold f n arr@(DArray sh@(sh' :*: s) fn) = 
  DArray sh' f'
  where
    f' i = U.fold f n (U.map (\(():*:s)-> fn (i:*:s)) (Array.range (():*:s)))

{-

da_mapFold:: (U.Elt e, A.Shape dim) => 
  (e -> e-> e) -> e -> DArray (dim :*: Int) e  -> DArray dim  e
{-# INLINE da_mapFold #-}
da_mapFold f n arr@(DArray sh@(sh' :*: s) fn) = toDArray $
  A.Array{ A.arrayShape = sh'
         , A.arrayData  = U.fold_s f n  (U.lengthsToSegd $ U.replicate noOfSegs s) 
           (U.map fn (A.range sh))}
  where
    noOfSegs = (A.size sh) `div` s
-}

    

----  Non-primitive functions 
----

da_shift:: (Array.Shape dim, U.Elt e) => DArray dim e -> e -> dim -> DArray dim e
{-# INLINE da_shift #-}
da_shift arr@(DArray sh _) e shiftOffset = da_backpermuteDft arr  e sh
  (\d -> if (Array.inRange sh (Array.addDim d shiftOffset)) then Just (Array.addDim d shiftOffset) else Nothing)

da_reshape:: (Array.Shape dim, Array.Shape dim', U.Elt e) => DArray dim e -> dim' -> DArray dim' e
da_reshape arr@(DArray sh fn) newShape = assert (Array.size newShape == Array.size sh) $
  DArray newShape (fn .  (Array.indexInv sh). (Array.index newShape))



da_rotate:: (Array.Shape dim, U.Elt e) => DArray dim e -> e -> dim -> DArray dim e
{-# INLINE da_rotate #-}
da_rotate arr@(DArray sh _) e shiftOffset = da_backpermute arr  sh
  (\d -> Array.addModDim sh d shiftOffset)



da_tile::  (Array.Shape dim, U.Elt e) => DArray dim e -> dim -> dim -> DArray dim e
{-# INLINE da_tile #-}
da_tile arr@(DArray sh _) start size = 
--  assert (Array.inRange sh (Array.addDim start size)) $
     da_backpermute arr size 
     (\d -> Array.addDim d start)


--  Combining arrays
-- 

--   
--
da_append:: (Array.Shape dim, U.Elt e) => DArray dim e -> DArray dim e -> dim -> DArray dim e
{-# INLINE da_append #-}
da_append arr1@(DArray sh1 fn1) arr2@(DArray sh2 fn2) newSh =
  DArray newSh appFn
  where
    appFn i = if (Array.inRange sh1 i) 
                then fn1 i
                else fn2 (Array.modDim i sh1)
  

--  Shape polymorphic ops based on Index
--  ====================================


da_select:: (U.Elt e, Array.Shape dim, Array.Shape dim') => 
  DArray dim e -> Array.SelectIndex dim dim'  -> DArray dim' e
{-# INLINE da_select #-}
da_select arr@(DArray shape _ ) ind = 
  da_backpermute arr (Array.projShape ind shape) (selectFun ind)
  where
    selectFun:: Array.SelectIndex dim1 dim2 -> dim2 -> dim1
    selectFun Array.IndexNil () = ()
    selectFun (Array.IndexAll rsh) (shs :*: s) = (selectFun rsh shs) :*: s
    selectFun (Array.IndexFixed n rsh) shs     = (selectFun rsh shs) :*: n

da_replicate:: (U.Elt e, Array.Shape dim, Array.Shape dim') => 
  DArray dim' e -> Array.SelectIndex dim dim'  -> DArray dim e
{-# INLINE da_replicate #-}
da_replicate arr@(DArray shape _ ) ind = 
  da_backpermute arr (Array.initShape ind shape) (repFun ind)
  where
    repFun:: Array.SelectIndex dim1 dim2 -> dim1 -> dim2
    repFun Array.IndexNil () = ()
    repFun (Array.IndexAll rsh) (shs :*: s) = (repFun rsh shs) :*: s
    repFun (Array.IndexFixed _ rsh) (shs :*: _) = repFun rsh shs


da_index::(U.Elt e, Array.Shape dim) => DArray dim e -> dim -> e
{-# INLINE da_index #-}
da_index arr@(DArray _ fn) i =
    fn i  


-- uses the 'standard' library functions
da_transpose:: U.Elt e => DArray Array.DIM2 e -> DArray Array.DIM2 e
{-# INLINE da_transpose #-}
da_transpose arr@(DArray (() :*:n :*: m) fn) = 
  da_backpermute arr  (() :*: m :*: n) (\((() :*: i) :*: j) -> ((() :*: j) :*: i))


da_mmMult:: DArray Array.DIM2 Double -> DArray Array.DIM2 Double -> DArray Array.DIM2 Double
da_mmMult arr1@(DArray (() :*: m1 :*: n1) fn1) arr2@(DArray (() :*: m2 :*: n2) fn2) = 
  assert (m1 == n2) $ 
  da_mapFold (+) 0 $ da_zipWith (*) arr1Ext arr2Ext
  where
    arr1Ext = da_replicate arr1 (Array.IndexAll (Array.IndexFixed m2 (Array.IndexAll Array.IndexNil)))
    arr2Ext = da_replicate 
                (da_transpose arr2) (Array.IndexAll (Array.IndexAll (Array.IndexFixed n1 Array.IndexNil)))


da_relaxShift:: Array.Array Array.DIM2 Double -> Array.Array Array.DIM2 Double
{-# INLINE da_relaxShift #-}
da_relaxShift arr' =  fromDArray $
  da_map ( (/) 5) $ 
     da_zipWith (+) 
    (da_zipWith (+) (da_zipWith (+) shiftu arr) shiftl) (da_zipWith (+) shiftr shiftd)
  where
    arr = toDArray arr'
    s@((() :*: n) :*: m) = Array.arrayShape arr'
    shiftu = da_shift arr 0 (():*: 1   :*:0)
    shiftd = da_shift arr 0 (():*:(-1) :*:0)
    shiftl = da_shift arr 0 (():*: 0   :*:1)
    shiftr = da_shift arr 0 (():*: 0   :*:(-1))


--
--  Hierarchical
--  =============


data HMatrix a =  HMatrix { hmThreshold:: Int
                          , hmOrder    :: Int
                          , hmDA       :: Array.DArray Array.DIM2 a
                          }


-- Just changes the representation, not the ordering of the elements.
--
toHMatrix:: (U.Elt a) => Int -> Array.Array Array.DIM2 a -> HMatrix a
{-# INLINE toHMatrix #-}
toHMatrix threshold arr =
   HMatrix{ hmThreshold =  threshold
          , hmOrder     =  order
          , hmDA        =  Array.toDArray  arr
          }
   where
     (_:*: order) = Array.arrayShape arr


 

($$):: (Array.DArray Array.DIM2 a -> b) -> HMatrix a -> b
($$) f hm = f $ hmDA hm

hmSplit:: U.Elt a => HMatrix a -> (HMatrix a, HMatrix a, HMatrix a, HMatrix a)
{-# INLINE hmSplit #-}
hmSplit hm = assert (hmOrder hm > 1) $
  (t1,t2,t3,t4)
  where
    n2 = (hmOrder hm) `div` 2
    t1 = hm{ hmOrder = n2, hmDA    = (Array.da_tile $$ hm) (():*: 0 :*: 0)   (() :*: n2 :*: n2)}
    t2 = hm{ hmOrder = n2, hmDA    = (Array.da_tile $$ hm) (():*: n2 :*: 0)  (() :*: n2 :*: n2)}
    t3 = hm{ hmOrder = n2, hmDA    = (Array.da_tile $$ hm) (():*: 0 :*: n2)  (() :*: n2 :*: n2)}
    t4 = hm{ hmOrder = n2, hmDA    = (Array.da_tile $$ hm) (():*: n2 :*: n2) (() :*: n2 :*: n2)}

hmJoin:: U.Elt a => (HMatrix a, HMatrix a, HMatrix a, HMatrix a) -> HMatrix a 
{-# INLINE hmJoin #-}
hmJoin (t1,t2,t3,t4) = assert (hmOrder t1 > 1) $
    t1{hmDA = Array.da_append (Array.da_append (hmDA t1) (hmDA t2) (() :*: 2 * order :*: order)) 
                        (Array.da_append (hmDA t3) (hmDA t4) (() :*: 2 * order :*: order))
                        (() :*: 2* order :*: 2* order)}
  where
    order = hmOrder t1    



--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hmmult:: HMatrix Double -> HMatrix Double -> HMatrix Double
hmmult a b = 
    if (n <= hmThreshold a) 
    then a{hmDA = Array.da_mmMult (toDArray$ fromDArray$ hmDA a) (toDArray$ fromDArray$ hmDA b)}
    else hmJoin (r1,r2,r3,r4)

  where
    n = hmOrder a
    m = hmOrder b

    (a1,a2,a3,a4) = hmSplit a
    (b1,b2,b3,b4) = hmSplit b
  
    r1 = a1{hmDA = (Array.da_zipWith (+) $$ (hmmult a1 b1)) $$ (hmmult a2 b3)}
    r2 = a2{hmDA = (Array.da_zipWith (+) $$ (hmmult a1 b2)) $$ (hmmult a2 b4)}
    r3 = a3{hmDA = (Array.da_zipWith (+) $$ (hmmult a3 b1)) $$ (hmmult a4 b3)}
    r4 = a4{hmDA = (Array.da_zipWith (+) $$ (hmmult a3 b2)) $$ (hmmult a4 b4)}

--
--  Hierarchical (one dim)
--  ======================


data HHMatrix a =  HHMatrix { hhmThreshold:: Int
                            , hhmOrder    :: Int
                            , hhmDA       :: Array.DArray Array.DIM1 a
                            }


-- Just changes the representation, not the ordering of the elements.
--
toHHMatrix:: (U.Elt a) => Int -> Int -> Array.Array Array.DIM1 a -> HHMatrix a
{-# INLINE toHHMatrix #-}
toHHMatrix order threshold arr =
   HHMatrix{ hhmThreshold =  threshold
          , hhmOrder     =  order
          , hhmDA        =  Array.toDArray  arr
          }



 

($$$):: (Array.DArray Array.DIM1 a -> b) -> HHMatrix a -> b
($$$) f hm = f $ hhmDA hm

hhmSplit:: U.Elt a => HHMatrix a -> (HHMatrix a, HHMatrix a, HHMatrix a, HHMatrix a)
{-# INLINE hhmSplit #-}
hhmSplit hm = assert (hhmOrder hm > 1) $
  (t1,t2,t3,t4)
  where
    n2 = (hhmOrder hm) `div` 2
    t1 = hm{ hhmOrder = n2, hhmDA    = (Array.da_tile $$$ hm) (():*: 0)       (() :*: n2*n2)}
    t2 = hm{ hhmOrder = n2, hhmDA    = (Array.da_tile $$$ hm) (():*: n2*n2)   (() :*: n2*n2)}
    t3 = hm{ hhmOrder = n2, hhmDA    = (Array.da_tile $$$ hm) (():*: 2*n2*n2) (() :*: n2*n2)}
    t4 = hm{ hhmOrder = n2, hhmDA    = (Array.da_tile $$$ hm) (():*: 3*n2*n2) (() :*: n2*n2)}

hhmJoin:: U.Elt a => (HHMatrix a, HHMatrix a, HHMatrix a, HHMatrix a) -> HHMatrix a 
{-# INLINE hhmJoin #-}
hhmJoin (t1,t2,t3,t4) = assert (hhmOrder t1 > 1) $
    t1{hhmDA = Array.da_append 
                (Array.da_append (hhmDA t1) (hhmDA t2) (() :*: 2*order*order)) 
                (Array.da_append (hhmDA t3) (hhmDA t4) (() :*: 2*order*order))
                        (() :*: 4*order*order)}
  where
    order = hhmOrder t1    

--  matrix multiplication, assumes square matrices of size 2^n * 2^n
--
hhmmult:: HHMatrix Double -> HHMatrix Double -> HHMatrix Double
hhmmult a b = 
    if (n <= hhmThreshold a) 
    then a{hhmDA = da_reshape
                    (Array.da_mmMult (toDArray$ reshape (fromDArray$ hhmDA a) (():*: n:*:n)) 
                                     (toDArray$ reshape (fromDArray$ hhmDA b) (():*: n:*:n)))
                    (() :*: n*n) 
          }
    else hhmJoin (r1,r2,r3,r4)

  where
    n = hhmOrder a
    m = hhmOrder b

    (a1,a2,a3,a4) = hhmSplit a
    (b1,b2,b3,b4) = hhmSplit b
  
    r1 = a1{hhmDA = (Array.da_zipWith (+) $$$ (hhmmult a1 b1)) $$$ (hhmmult a2 b3)}
    r2 = a2{hhmDA = (Array.da_zipWith (+) $$$ (hhmmult a1 b2)) $$$ (hhmmult a2 b4)}
    r3 = a3{hhmDA = (Array.da_zipWith (+) $$$ (hhmmult a3 b1)) $$$ (hhmmult a4 b3)}
    r4 = a4{hhmDA = (Array.da_zipWith (+) $$$ (hhmmult a3 b2)) $$$ (hhmmult a4 b4)}

