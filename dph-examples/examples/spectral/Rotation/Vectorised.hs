{-# LANGUAGE ParallelArrays, ParallelListComp #-}
{-# OPTIONS -fvectorise #-}

module Vectorised
    (solvePA
    , pi
    , solveV2
    )
where
import Data.Array.Parallel hiding ((+), (-), (*), (/))
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Prelude.Bool
import Data.Array.Parallel.Prelude.Double        as D hiding (pi)
import qualified Data.Array.Parallel.Prelude.Int as I
import qualified Prelude    as P

{-
data NodeV = NodeV Double Double Double [:NodeV:]
-}

pi = 3.1415926535
type Point = (Double,Double)

{-# NOINLINE solvePA #-}
solvePA
    :: Int      -- ^ depth
    -> Double   -- ^ time
    -> PArray Point
solvePA depth t
 = let s p   = solveV2 t depth p
   in toPArrayP (s 0 +:+ s (pi/2) +:+ s pi +:+ s (3*pi / 2))

solveV2 :: Double -> Int -> Double -> [:Point:]
solveV2 t iG pG
 = let 
       {-# INLINE l #-}
       l         = fromInt iG
       {-# INLINE p #-}
       p         = pG
       {-# INLINE f #-}
       f         = fromInt iG / 20

       
       {-# INLINE r' #-}
       r'        = p + f*t
       cos'      = cos r'
       sin'      = sin r'
       (px, py)  = (- l * sin', l * cos')

       {-# INLINE pts #-}
       pts       = concatP (mapP (\iG2 -> solveV2 t (iG I.- 1) (fromInt iG2 / l * 2 * pi - pi)) (I.enumFromToP 1 iG))
       {-# INLINE pts' #-}
       pts'      = mapP (\(x,y) -> (x * cos' - y * sin' + px, x * sin' + y * cos' + py)) pts
   in singletonP (px, py) +:+ pts'


{-
mkNode :: Int -> Double -> NodeV
mkNode i p
 = let i' = fromInt i
   in  NodeV i' p (i' / 20)
       (mapP (\i2 -> mkNode (i I.- 1) (fromInt i2 / i' * 2 * pi - pi)) (I.enumFromToP 1 i))


{-# INLINE solveV #-}
solveV :: Double -> NodeV -> [:Point:]
solveV t (NodeV l p f c)
 = let r'        = p + f*t
       cos'      = cos r'
       sin'      = sin r'
       rot (x,y) = (x * cos' - y * sin', x * sin' + y * cos')
       (px, py)  = rot (0, l)
       trn (x,y) = (x + px, y + py)

       pts       = concatP (mapP (solveV t) c)
       pts'      = mapP (\p -> trn (rot p)) pts
   in singletonP (px, py) +:+ pts'

-}
