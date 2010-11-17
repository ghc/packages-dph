
module World
where
import Points2D.Types
import Points2D.Generate
import Geometry.Intersection
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Data.Maybe


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


-- | Split segments that cross the line y = y0, for some y0.
splitSegmentsOnY :: Double -> Vector Segment -> Vector Segment
splitSegmentsOnY y0 segs
 = let	
	-- TODO: we only need to know IF the seg crosse the line here,
	--       not the actual intersection point. Do a faster test.
	(segsCross, segsOther)
		= V.unstablePartition 
			(\(_, p1, p2) -> isJust $ intersectSegHorzLine p1 p2 y0)
			segs

	-- TODO: going via lists here is bad.
	splitCrossingSeg :: Segment -> Vector Segment
	splitCrossingSeg (n, p1, p2)
	 = let	Just pCross	= intersectSegHorzLine p1 p2 y0
	   in	V.fromList [(n, p1, pCross), (n, pCross, p2)]
	
	-- TODO: vector append requires a copy.	
   in	segsOther V.++ (V.concat $ map splitCrossingSeg $ V.toList segsCross)


-- | Translate both endpoints of a segment.
translateSegment :: Double -> Double -> Segment -> Segment
translateSegment tx ty (n, (x1, y1), (x2, y2))
	= (n, (x1 + tx, y1 + ty), (x2 + tx, y2 + ty))


-- | Normalise the world so that the given point is at the origin,
--   and split segements that cross the y=0 line.
normaliseWorld :: Point -> World -> World
normaliseWorld (px, py) world
 = let	segments_trans	= V.map (translateSegment (-px) (-py)) 
			$ worldSegments world
			
	segments_split	= splitSegmentsOnY 0 segments_trans
			
   in	world { worldSegments = segments_split }
	
