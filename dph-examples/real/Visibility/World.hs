
module World
where
import Points2D.Generate
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)

-- We keep this unpacked so we can use unboxed vector.
-- index, x1, y1, x2, y2
type Segment
	= (Int, (Double, Double), (Double, Double))
	
data World 
	= World
	{ worldSegments	:: Vector Segment }

-- | Generate the initial world.
initialWorld :: IO World
initialWorld
 = do	let n		= 1000
	let minZ	= -400
	let maxZ	= 400
	
	let minDelta	= -50
	let maxDelta	= 50
	
	let centers	= genPointsUniform n minZ maxZ
	let deltas	= genPointsUniformWithSeed 4321 n minDelta maxDelta

	let makePoint n' (cX, cY) (dX, dY)
			= (n', (cX, cY), (cX + dX, cY + dY))

	let segs	= V.zipWith3 makePoint (V.enumFromTo 0 (n - 1)) centers deltas
	
	return $ World segs

{-
-- | Split segments that cross the line y = y0, for some y0.
splitSegmentsOnY :: Double -> Vector Segment -> Vector Segment
splitSegmentsOnY y0 segs


splitSegment :: 
-}	


translateSegment :: Double -> Double -> Segment -> Segment
translateSegment tx ty (n, (x1, y1), (x2, y2))
	= (n, (x1 + tx, y1 + ty), (x2 + tx, y2 + ty))

	
