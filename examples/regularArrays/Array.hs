{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts  #-}
{-# LANGUAGE UndecidableInstances, TypeOperators,TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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

module Array where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))
--import Control.Exception



import Debug.Trace

type ElemT = Double

assert a b = b
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


class RepFun dim1 where
  repFun:: SelectIndex dim1 dim2 ->  dim1 -> dim2 

instance RepFun () where
  {-# INLINE repFun #-}
  repFun IndexNil () = () 

instance RepFun dim1  => RepFun (dim1 :*: Int)  where
-- @Roman:
-- adding the inline pragma slows the code down by a factor of 2 for the
-- delayed mmult for example
  {-# INLINE repFun #-}
  repFun (IndexAll rsh) (shs :*: s)     = (repFun rsh shs) :*: s
  repFun (IndexFixed _ rsh) (shs :*: _) = repFun rsh shs

class InitShape dim where
  initShape:: SelectIndex dim dim' -> dim' -> dim

instance InitShape () where
  initShape IndexNil () = ()

instance InitShape dim => InitShape (dim :*: Int) where
  initShape (IndexFixed n rsh) shs       = (initShape rsh shs) :*: n
  initShape (IndexAll rsh) (shs :*: s)   = (initShape rsh shs) :*: s

-- |Our index class
--
class (Show sh, U.Elt sh, Eq sh) => Shape sh where
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
  {-# INLINE addDim #-}
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
    let (r,i) = n `divMod` d 
    in (indexInv ds r) :*: i

      

-- adding this inline pragma make hmmult very slow
  {-# INLINE addDim #-}
  addDim (sh1 :*: n1) (sh2 :*: n2) = ((addDim sh1 sh2) :*: (n1 + n2))
  addModDim (aSh :*: a) (bSh :*: b) (cSh :*: m) =
    (addModDim aSh bSh cSh :*: ((a + b + 1) `mod` m) -1)


  modDim (sh1 :*: n1) (sh2 :*: n2) = (modDim sh1 sh2 :*: (n1 `mod` n2))
  {-# INLINE range #-}
  range (sh :*: n) = U.zipWith (\r -> \t -> (r :*: t)) 
    (U.replicate_s (U.lengthsToSegd  $ U.replicate (size sh) n) (range sh))
    (U.repeat (size sh) n (U.enumFromTo 0 (n-1)))

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

replicate:: (U.Elt e, Shape dim, Shape dim', RepFun dim, InitShape dim) => 
  Array dim' e ->SelectIndex dim dim'  -> Array dim e
--{-# INLINE replicate #-}
replicate arr ind = -- trace (show $ (initShape ind (arrayShape arr))) $
  backpermute arr (initShape ind (arrayShape arr)) (repFun ind)




-- Given a selector index and the initial dimension, calculate the dim of
-- the resulting projection 
projShape:: (Shape dim, Shape dim') => SelectIndex dim dim' -> dim -> dim'
projShape IndexNil () = ()
projShape (IndexAll ixs)     (shs :*: s) = (projShape ixs shs) :*: s
projShape (IndexFixed _ ixs) (shs   :*: s) = projShape ixs shs


-- Computations
-- ============
map:: (U.Elt a, U.Elt b, Shape dim) => (a -> b) -> Array dim a -> Array dim b
{-# INLINE map #-}
map f arr = arr{arrayData = U.map f $ arrayData arr}


zipWith:: (U.Elt a, U.Elt b, U.Elt c, Shape dim) => 
  (a -> b -> c) -> Array dim a -> Array dim b-> Array dim c
{-# INLINE zipWith #-}
zipWith f arr1 arr2 = arr1{arrayData = U.zipWith f (arrayData arr1) (arrayData arr2)}


fold :: (U.Elt e, Shape dim) => (e -> e-> e) -> e -> Array dim e  -> e
{-# INLINE fold #-}
fold f n arr = 
  U.fold f n  $ arrayData arr


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



