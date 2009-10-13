{-# LANGUAGE GADTs, TypeFamilies, FlexibleInstances, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances #-}

module DArrayExamples ( 
    transpose
  , mmMult
  , mmMult'
  , relaxMS
  , relaxShift
  , fft3d
  ) where

import Prelude hiding (map, zip, zipWith, replicate)
import qualified Prelude (map)
import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Unlifted ((:*:)(..))

import qualified Array 
import DArray


assert a b = b 

transpose:: U.Elt e => DArray Array.DIM2 e -> DArray Array.DIM2 e
{-# INLINE transpose #-}
transpose arr@(DArray (() :*:n :*: m) fn) = 
  backpermute arr  (() :*: m :*: n) (\((() :*: i) :*: j) -> ((() :*: j) :*: i))




mmMult' m1 m2 = fromDArray $ mmMult m1 m2 



mmMult:: DArray Array.DIM2 Double -> DArray Array.DIM2 Double -> DArray Array.DIM2 Double
mmMult arr1@(DArray (() :*: m1 :*: n1) fn1) arr2@(DArray (() :*: m2 :*: n2) fn2) = 
  assert (m1 == n2) $ 
  mapFold (+) 0 $ zipWith (*) arr1Ext arr2Ext
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
    shiftu = shift arr 0 (():*: 1   :*:0)
    shiftd = shift arr 0 (():*:(-1) :*:0)
    shiftl = shift arr 0 (():*: 0   :*:1)
    shiftr = shift arr 0 (():*: 0   :*:(-1))


relaxMS:: DArray Array.DIM2 Double -> DArray Array.DIM2 Double
{-# INLINE relaxMS #-}
relaxMS arr@(DArray (() :*: n :*:m) fn) = 
  mapStencil border (() :*: 5) stencil id sumD arr
  where
    sumD arr = (fold (+) 0 arr)/5
    border:: Array.DIM2 -> Bool
    border (() :*: n' :*: m') = (n' == 0) || (n' >= n) ||
                                (m' == 0) || (m' >= m) 

    stencil:: Array.DIM2 -> Array.DIM1 -> Array.DIM2
    stencil (() :*: i :*:j) (() :*: 0) = (() :*: (i-1) :*: j)
    stencil (() :*: i :*:j) (() :*: 1) = (() :*: i :*: (j-1))
    stencil (() :*: i :*:j) (() :*: 2) = (() :*: (i+1) :*: j)
    stencil (() :*: i :*:j) (() :*: 3) = (() :*: i :*: (j+1))
    stencil (() :*: i :*:j) (() :*: 4) = (() :*: i :*: j)
    


--  Red/Black 3d relaxation
--  -----------------------


combineDArrays:: (U.Elt e, Array.Shape dim) => DArray dim Bool -> DArray dim e -> DArray dim e -> DArray dim e
{-# INLINE combineDArrays #-}
combineDArrays mask as bs = 
  map (\(m :*: (a :*: b)) -> if m then a else b)  $ zip mask (zip as bs)

--  Red-Black relaxation
-- ----------------------


redBlack:: Double -> Double -> DArray Array.DIM3 Double -> DArray Array.DIM3 Double -> DArray Array.DIM3 Double
redBlack factor hsq f arr@(DArray (() :*: l :*: n :*:m) fn)  = 
  applyFactor $
  mapStencil (isBorder &. isRed) (() :*: 6) stencil id sumD $
  applyFactor $ 
  mapStencil (isBorder &. (not . isRed)) (() :*: 6) stencil id sumD arr
  where
    (&.) p1 p2 x = (p1 x) && (p2 x)

    applyFactor:: DArray Array.DIM3 Double -> DArray Array.DIM3 Double
    applyFactor = zipWith (\fi -> \si -> factor *  (hsq * fi + si)) f

    sumD arr = fold (+) 0 arr 
 
    isRed:: Array.DIM3 -> Bool
    isRed (_ :*: j) = even j

    isBorder:: Array.DIM3 -> Bool
    isBorder (() :*: h :*: i :*: j) = ((h * i * j) == 0)  || 
      (h >= l) || (i >= m) || (j >= n)

    stencil:: Array.DIM3 -> Array.DIM1 -> Array.DIM3
    stencil (() :*: h :*: i :*:j) (() :*: 0) = (() :*: h :*: (i-1) :*: j)
    stencil (() :*: h :*: i :*:j) (() :*: 1) = (() :*: h :*: i :*: (j-1))
    stencil (() :*: h :*: i :*:j) (() :*: 2) = (() :*: h :*: (i+1) :*: j)
    stencil (() :*: h :*: i :*:j) (() :*: 3) = (() :*: h :*: i :*: (j+1))
    stencil (() :*: h :*: i :*:j) (() :*: 4) = (() :*: (h-1) :*: i :*: j)
    stencil (() :*: h :*: i :*:j) (() :*: 5) = (() :*: (h+1) :*: i :*: j)


--  FFT example
-- -------------

type Complex = (Double :*: Double)

instance Num Complex where
  (r :*: i) + (r' :*: i') = (r+r' :*: i+i')
  (r :*: i) - (r' :*: i') = (r-r' :*: i-i')
  (r :*: i) * (r' :*: i') = (r*r' - i*i' :*: r*i' + r'*i)
  fromInteger n = (fromInteger n :*: 0.0)


fft3d:: Int -> DArray Array.DIM1 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
fft3d it rofu  m | it < 1    = m
                 | otherwise = fft3d (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = (mapFFT rofu) . transpose'
    transpose' darr@(DArray (() :*: k :*: l :*: m) _) = 
      backpermute darr (() :*: m :*: k :*: l)
            (\(() :*: m' :*: k' :*: l') -> (() :*: k' :*: l' :*: m')) 




-- precondition:
--   length of rofu is half of length of f
fft:: DArray Array.DIM1 Complex -> DArray Array.DIM1 Complex -> DArray Array.DIM1 Complex
fft rofu  v@(DArray (() :*: n) f) 
  | n > 2     = append (zipWith (+) fft_left fft_right) (zipWith (-) fft_left fft_right) (() :*: n)
  | otherwise = DArray (() :*: n) f'
  where 
    f' (() :*: 0) = f (() :*: 0) + f (() :*: 1)
    f' (() :*: 1) = f (() :*: 0) - f (() :*: 1)
    rofu'     = select' rofu (\(():*: i) -> (() :*: 2*i))
    fft_left  = forceDArray $ zipWith (*) rofu $ fft rofu' (select' v (\(():*: i) -> (() :*: 2*i))) 
    fft_right = forceDArray $ fft rofu' (select' v (\(():*: i) -> (() :*: 2*i+1))) 

select':: DArray Array.DIM1 Complex -> (Array.DIM1 -> Array.DIM1) -> DArray Array.DIM1 Complex
select' arr@(DArray (() :*: n) fn) sel =
  (DArray (() :*: (n `div` 2)) (fn . sel)) 


mapFFT:: DArray Array.DIM1 Complex -> DArray Array.DIM3 Complex -> DArray Array.DIM3 Complex
mapFFT rofu arr = 
  ndimConc $ Prelude.map ndimConc $ Prelude.map (Prelude.map (fft rofu)) $ 
             Prelude.map ndimExtr $ ndimExtr arr


-- The following two functions are just temporary solutions: will be implemented
-- as nested arrays of regular array element type, with a non-vectorised map
ndimExtr:: (U.Elt e, Array.Shape dim) => DArray (dim :*: Int) e -> [DArray dim e] 
ndimExtr (DArray (sh :*: n)  fn) = Prelude.map mkDArr [0..(n-1)] 
  where
    mkDArr i = DArray sh (fn' i)
    fn' i sh  = fn (sh :*: i) 

ndimConc:: (U.Elt e, Array.Shape dim) => [DArray dim e] -> DArray (dim :*: Int) e
ndimConc arrs@(DArray sh _ : _) = DArray ((foldl accSh sh arrs):*: n) fn
  where
    n  = length arrs
    accSh sh' (DArray sh _) | sh == sh' = sh
                            | otherwise = error "ndimConc on heterogeneous arrays"
    fn (sh :*: n) = let (DArray _ f) = arrs!!n in f sh                          
   

  
