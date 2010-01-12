{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances #-}

module DArrayExamples ( 
    transpose
  , mmMult
  , mmMult'
  , relaxMS
  , relaxShift
  , redBlack
  , fft3D 
  ) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))

import qualified Array 
import DArray

import Prelude hiding (map, zip, zipWith, replicate)
import qualified Prelude (map)

import Debug.Trace
import Control.Exception (assert)


transpose:: (Array.Shape dim, U.Elt e) => 
  DArray (dim :*: Int :*: Int) e -> DArray (dim :*: Int :*: Int) e
{-# INLINE transpose #-}
transpose arr@(DArray (sh :*:n :*: m) fn) = 
  backpermute arr  (sh :*: m :*: n) (\((sh' :*: i) :*: j) -> ((sh' :*: j) :*: i))




mmMult' m1 m2 = fromDArray $ mmMult m1 m2 


-- the polymorhic version is significantly slower (not surprisingly), but so is the (pragma) specialised version. Why???
{-# SPECIALIZE mmMult::
   DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
  #-}
-- mmMult:: (Array.RepFun dim, Array.InitShape dim, Array.Shape dim) => 
--   DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double -> DArray (dim :*: Int :*: Int)  Double  
mmMult::
   DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double -> DArray (() :*: Int :*: Int)  Double  
mmMult arr1@(DArray (sh :*: m1 :*: n1) fn1) arr2@(DArray (sh' :*: m2 :*: n2) fn2) = 
  assert ((m1 == n2) && (sh == sh')) $ 
    mapFold (+) 0 (arr1Ext * arr2Ext)
--  'fold' doesn't fuse at the moment, so mapFold is significantly faster
--  fold (+) 0 $ zipWith (*) arr1Ext arr2Ext
  where
    arr2T   = forceDArray $ transpose arr2  -- forces evaluation of 'transpose'
    arr1Ext = replicate arr1 (Array.IndexAll (Array.IndexFixed m2 (Array.IndexAll Array.IndexNil)))
    arr2Ext = replicate arr2T
                 (Array.IndexAll (Array.IndexAll (Array.IndexFixed n1 Array.IndexNil)))



relaxShift:: DArray Array.DIM2 Double -> Array.Array Array.DIM2 Double
{-# INLINE relaxShift #-}
relaxShift arr =  fromDArray $ map (\(a :*: b :*: c :*: d :*: e) -> (a+b+c+d+e)/5) $ 
    zip (zip (zip (zip shiftu shiftd) shiftl) shiftr) arr
  where
--    arr = toDArray arr'
    s@(DArray ((() :*: n) :*: m) _) = arr
    shiftu = shift arr 0 ((():*: 1   :*:0)::Array.DIM2)
    shiftd = shift arr 0 ((():*:(-1) :*:0)::Array.DIM2)
    shiftl = shift arr 0 ((():*: 0   :*:1)::Array.DIM2)
    shiftr = shift arr 0 ((():*: 0   :*:(-1))::Array.DIM2)


relaxMS:: Array.Shape dim => DArray (dim :*: Int :*: Int) Double -> DArray (dim :*: Int :*: Int) Double
{-# INLINE relaxMS #-}
relaxMS arr@(DArray (sh :*: n :*:m) fn) = 
  mapStencil border (() :*: 5) stencil id sumD arr
  where
    sumD arr = (toScalar $ (mapFold (+) 0 arr))/5
--    border:: Array.DIM2 -> Bool
    border  (_ :*: n' :*: m') = (n' == 0) || (n' >= n) ||
                                (m' == 0) || (m' >= m) 

--    stencil:: Array.DIM2 -> Array.DIM1 -> Array.DIM2
    stencil (sh :*: i :*:j) (sh' :*: 0) = (sh :*: (i-1) :*: j)
    stencil (sh :*: i :*:j) (sh' :*: 1) = (sh :*: i :*: (j-1))
    stencil (sh :*: i :*:j) (sh' :*: 2) = (sh :*: (i+1) :*: j)
    stencil (sh :*: i :*:j) (sh' :*: 3) = (sh :*: i :*: (j+1))
    stencil (sh :*: i :*:j) (sh' :*: 4) = (sh :*: i :*: j)
    


--  Red/Black 3d relaxation
--  -----------------------


combineDArrays:: (U.Elt e, Array.Shape dim) => DArray dim Bool -> DArray dim e -> DArray dim e -> DArray dim e
{-# INLINE combineDArrays #-}
combineDArrays mask as bs = 
  map (\(m :*: (a :*: b)) -> if m then a else b)  $ zip mask (zip as bs)

--  Red-Black relaxation
-- ----------------------


redBlack:: Array.Shape dim => Double -> Double -> DArray (dim :*: Int :*: Int :*: Int) Double ->
             DArray  (dim :*: Int :*: Int :*: Int) Double -> DArray (dim :*: Int :*: Int :*: Int) Double
redBlack factor hsq f arr@(DArray (d :*: l :*: n :*:m) fn)  = 
  applyFactor $
  mapStencil (isBorder &. isRed) (() :*: 6) stencil id sumD $
  applyFactor $ 
  mapStencil (isBorder &. (not . isRed)) (() :*: 6) stencil id sumD arr
  where
    (&.) p1 p2 x = (p1 x) && (p2 x)

--    applyFactor:: DArray Array.DIM3 Double -> DArray Array.DIM3 Double
    applyFactor = zipWith (\fi -> \si -> factor *  (hsq * fi + si)) f
    
    sumD:: DArray (() :*: Int) Double ->  Double
    sumD arr = toScalar $ mapFold (+) 0 arr 
 
--    isRed:: Array.DIM3 -> Bool
    isRed (_ :*: j) = even j

--    isBorder:: Array.DIM3 -> Bool
    isBorder (d :*: h :*: i :*: j) = ((h * i * j) == 0)  || 
      (h >= l) || (i >= m) || (j >= n)

--     stencil:: Array.DIM3 -> Array.DIM1 -> Array.DIM3
    stencil (d :*: h :*: i :*:j) (_ :*: 0) = (d :*: h :*: (i-1) :*: j)
    stencil (d :*: h :*: i :*:j) (_ :*: 1) = (d :*: h :*: i :*: (j-1))
    stencil (d :*: h :*: i :*:j) (_ :*: 2) = (d :*: h :*: (i+1) :*: j)
    stencil (d :*: h :*: i :*:j) (_ :*: 3) = (d :*: h :*: i :*: (j+1))
    stencil (d :*: h :*: i :*:j) (_ :*: 4) = (d :*: (h-1) :*: i :*: j)
    stencil (d :*: h :*: i :*:j) (_ :*: 5) = (d :*: (h+1) :*: i :*: j)


--  FFT example
-- -------------

type Complex = (Double :*: Double)

instance Num Complex where
  (r :*: i) + (r' :*: i') = (r+r' :*: i+i')
  (r :*: i) - (r' :*: i') = (r-r' :*: i-i')
  (r :*: i) * (r' :*: i') = (r*r' - i*i' :*: r*i' + r'*i)
  fromInteger n = (fromInteger n :*: 0.0)


calcRofu:: Array.Shape dim =>  (dim :*: Int) -> DArray (dim :*: Int) Complex
calcRofu sh@(_ :*: n) = DArray sh f
  where
    f :: Array.Shape dim => (dim :*: Int) -> Complex
    f (_ :*: n) = ((cos (2 * pi/ ((fromIntegral n)+1))) :*: (sin  (2 * pi / ((fromIntegral n)+1))))




-- Calculates a vector of unity roots and calls 3D fft
fft3D:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex 
fft3D it m@(DArray (sh :*: n) _) =
  fft3d it (calcRofu (sh :*: size)) m
  where
    size ::  Int
    size = n `div` 2



fft3d:: Int -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3d it rofu  m | it < 1    = m
                | otherwise = fft3d (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = forceDArray . (fft rofu) . transpose'
    transpose' darr@(DArray (() :*: k :*: l :*: m) _) = 
      backpermute darr (() :*: m :*: k :*: l)
            (\(() :*: m' :*: k' :*: l') -> (() :*: k' :*: l' :*: m')) 


fft:: Array.Subshape dim  dim=> DArray (dim :*: Int) Complex -> DArray (dim :*: Int) Complex -> DArray (dim :*: Int) Complex 
fft rofu@(DArray ( _ :*: s) _ )  v@(DArray sh@(_ :*: n) f) 
  | n > 2     = assert (2 * s == n) $ 
    append (fft_left + fft_right) (fft_left - fft_right) sh
  | n == 2    = assert (2 * s == n) $ 
    DArray sh f'
  where 
    f' (sh :*: 0) = f (sh :*: 0) + f (sh :*: 1)
    f' (sh :*: 1) = f (sh :*: 0) - f (sh :*: 1)
    f' (sh :*: x) = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))

    rofu'     = split rofu (\(sh :*: i) -> (sh :*: 2*i))
    fft_left  = forceDArray $ rofu * (fft rofu' (split v (\(sh:*: i) -> (sh :*: 2*i))))
    fft_right = forceDArray $ fft rofu' (split v (\(sh:*: i) -> (sh :*: 2*i+1))) 
    
split:: Array.Shape dim => DArray (dim :*: Int) Complex -> ((dim :*: Int) -> (dim :*: Int)) -> DArray (dim :*: Int) Complex
split arr@(DArray (sh :*: n) fn) sel =
  (DArray (sh :*: (n `div` 2)) (fn . sel)) 

