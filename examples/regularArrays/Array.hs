{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances, TypeOperators,TypeSynonymInstances  #-}
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


type DIM0 = ()
type DIM1 = (DIM0 :*: Int)
type DIM2 = (DIM1 :*: Int)
type DIM3 = (DIM2 :*: Int)
type DIM4 = (DIM3 :*: Int)
type DIM5 = (DIM4 :*: Int)

data Index a initialDim projectedDim where
  IndexNil   :: Index a () ()
  IndexAll   :: (Shape init, Shape proj) =>      Index a init proj -> Index a (init :*: Int) (proj :*: Int)
  IndexFixed :: (Shape init, Shape proj) => a -> Index a init proj -> Index a (init :*: Int)  proj



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
  index sh n = 0

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



-- Matrix transposition
------------------------

-- uses the 'standard' library functions
transpose:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transpose #-}
transpose arr = 
  backpermute arr (() :*:n :*: m) (\((() :*: i) :*: j) -> ((() :*: j) :*: i))
  where
    (() :*:n :*: m) = arrayShape arr


-- avoids index/tuple calculations
transposePrim:: U.Elt e => Array DIM2 e -> Array DIM2 e
{-# INLINE transposePrim #-}
transposePrim arr = arr{arrayData = U.bpermute (arrayData arr) inds}
  where
   (_ :*: n) = arrayShape arr
   inds      = U.zipWith (+)
                 (U.repeat n (n*n) (U.enumFromStepLen 0 n n))
                 (U.replicate_s (U.lengthsToSegd  $ U.replicate n n) (U.enumFromTo 0 (n-1)))


-- uses default backpermute 
transposeDFT:: (U.Elt a, Num a) => Array DIM2 a ->        Array DIM2 a
{-# INLINE transposeDFT #-}
transposeDFT arr = assert (n==m) $
  backpermuteDft arr 0 (arrayShape arr) (\((() :*: i) :*: j) -> Just ((() :*: j) :*: i))
  where
    (() :*:n :*: m) = arrayShape arr



-- Matrix relaxation
-- -----------------

relax:: (U.Elt a, Fractional a) => Array DIM2 a -> Array DIM2 a
{-# INLINE relax #-}
relax arr =  
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

relaxShift:: Array DIM2 Double -> Array DIM2 Double
{-# INLINE relaxShift #-}
relaxShift arr =  
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


mmMult:: Array DIM2 Double -> Array DIM2 Double -> Array DIM2 Double
mmMult arr1 arr2 = assert (m1 == n2) $ 
  mapFold (+) 0 $ Array.zipWith (*) arr1Ext arr2Ext
  where
    arr1Ext = Array.replicate arr1 (IndexAll (IndexFixed m2 (IndexAll IndexNil)))
    arr2Ext = Array.replicate (transpose arr2) (IndexAll (IndexAll (IndexFixed n1 IndexNil)))
    (() :*: m1 :*: n1) = arrayShape arr1
    (() :*: m2 :*: n2) = arrayShape arr2

--  Alternative implementations for lib functions
--  ---------------------------------------------

myMap:: (U.Elt a, U.Elt b) => (a -> b) -> Array dim a -> Array dim b 
myMap f arr = arr{arrayData = U.mbpermute f arrd (U.enumFromTo 0 ((U.length arrd)-1))}
  where arrd = arrayData arr

myZipWith:: (U.Elt a, U.Elt b, U.Elt c) => (a -> b -> c) -> U.Array a -> U.Array b -> U.Array c
myZipWith f arr1 arr2 = U.mbpermute f' (U.zip arr1 arr2) (U.enumFromTo 0 ((U.length arr1)-1))
 where 
   f' (a :*: b) = f a b