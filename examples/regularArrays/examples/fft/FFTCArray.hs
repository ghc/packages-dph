{-# LANGUAGE TypeOperators, TypeSynonymInstances #-}
module FFTCArray ( fft3DC )
where

import Data.Array.Parallel.Base ( (:*:)(..) )
import CArray as CA
import Array ( Array, Shape, Subshape, DIM3, (:.)(..) )

import Control.Exception ( assert )

type Complex = Double :*: Double

instance Num Complex where
  (r :*: i) + (r' :*: i') = (r+r' :*: i+i')
  (r :*: i) - (r' :*: i') = (r-r' :*: i-i')
  (r :*: i) * (r' :*: i') = (r*r' - i*i' :*: r*i' + r'*i)
  fromInteger n = (fromInteger n :*: 0.0)

fft3DC:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex 
fft3DC it m@(CArray (sh :. n) _) =
  fft3dC it (calcRofu (sh :. size)) m
  where
    size ::  Int
    size = n `div` 2

fft3dC:: Int -> CArray DIM3 Complex -> CArray DIM3 Complex
                                    -> CArray DIM3 Complex
fft3dC it rofu  m  
    | it < 1    = m
    | otherwise = fft3dC (it-1) rofu $ fftTrans $ fftTrans $ fftTrans m 
  where
    fftTrans = transpose . (fftC rofu)
    transpose arr@(CArray (() :. k :. l :. m) _) = 
      backpermute arr (() :. m :. k :. l)
            (\(() :. m' :. k' :. l') -> (() :. k' :. l' :. m')) 

fftC:: Subshape dim dim => CArray (dim :. Int) Complex
                        -> CArray (dim :. Int) Complex
                        -> CArray (dim :. Int) Complex 
fftC rofu v
  | n <= 16 = fft rofu v
  | n > 2   = append (CA.zipWith (+) fft_left fft_right)
                     (CA.zipWith (-) fft_left fft_right) sh
  where
    sh     = carrayShape v
    _ :. n = sh

    rofu' = split rofu evens
    fft_left = forceCArray $ CA.zipWith (*) rofu (fftC rofu' (split v evens))
    fft_right = forceCArray $ fftC rofu' (split v odds)

    split arr sel = traverseCArray arr (\(sh :. i) -> sh :. (i `div` 2))
                                       (\f -> f . sel)

    evens (sh :. i) = sh :. 2*i
    odds  (sh :. i) = sh :. 2*i+1

calcRofu:: Shape dim => (dim :. Int) -> CArray (dim :. Int) Complex
calcRofu sh@(_ :. n) = forceCArray (genCArray sh f)
  where
    f :: Shape dim => (dim :. Int) -> Complex
    f (_ :. n) = cos (2 * pi/ ((fromIntegral n)+1)) :*: sin  (2 * pi / ((fromIntegral n)+1))

fft:: Subshape dim  dim => CArray (dim :. Int) Complex
                        -> CArray (dim :. Int) Complex
                        -> CArray (dim :. Int) Complex 
fft rofu v
  | vLen > 2     = 
      append fft_l fft_r  (carrayShape v)
  | vLen == 2    = assert (2 * rLen == vLen) $ 
        traverseCArray v id vFn'
  where 
    (_ :. vLen) = carrayShape v
    (_ :. rLen) = carrayShape rofu
    vFn' vFn (sh :. 0)  = vFn (sh :. 0) + vFn (sh :. 1)
    vFn' vFn (sh :. 1)  = vFn (sh :. 0) - vFn (sh :. 1)
    vFn' _   (sh :. x)  = error ("error in fft - f:" ++ (show x) ++ "/" ++ (show sh))

    fft_lr = forceCArray $ fft splitRofu splitV -- par

    splitRofu = 
      traverseCArray rofu
        (\(rSh :. rLen) -> rSh :. (2::Int) :. (rLen `div` 2))
        (\rFn (sh :. _ :. i) -> rFn (sh :. 2*i))
 
    splitV = traverseCArray v
      (\(vSh :. vLen) -> vSh :. 2 :. (vLen `div` 2)) vFn'
       where 
         vFn' vFn (sh :. 0 :. i) = vFn (sh :. 2*i)
         vFn' vFn (sh :. 1 :. i) = vFn (sh :. 2*i+1)


    fft_l = traverse2CArray fft_lr rofu 
             (\(sh :. 2 :. n) _ -> sh :. n)
             (\f r (sh :. i) -> f (sh:. 0 :. i) + r (sh :. i) * f (sh :. 1 :. i))

    fft_r = traverse2CArray fft_lr rofu 
             (\(sh :. 2 :. n) _ -> sh :. n)
             (\f r (sh :. i) -> f (sh:. 0 :. i) - f (sh :. 1 :. i))

