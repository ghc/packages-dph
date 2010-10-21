
-- | Naive n^2 computation of accelerations.
module Naive
	(calcAccels)
where
import Common
import Data.Vector.Unboxed		(Vector)
import qualified Data.Vector.Unboxed	as V


-- | Calculate accelerations on these point in a naive O(n^2) way
calcAccels :: Vector MassPoint -> Vector Accel
calcAccels mps
	= V.map (calcAccel mps) mps

calcAccel :: Vector MassPoint -> MassPoint -> Accel
calcAccel mps mp
 = let	(xs, ys)	= V.unzip $ V.map (accel mp) mps
   in	(V.sum xs, V.sum ys)
