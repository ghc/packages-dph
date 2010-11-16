
module World
where
import Points2D.Generate
import qualified Graphics.Gloss		as G
import qualified Graphics.Gloss.Shapes	as G
import qualified Graphics.Gloss.Game	as G
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)

-- We keep this unpacked so we can use unboxed vector.
-- index, x1, y1, x2, y2
type Segment
	= (Int, Double, Double, Double, Double)
	
data World 
	= World
	{ worldSegments	:: Vector Segment }

initialWorld :: IO World
initialWorld
 = do	let n		= 100
	let minZ	= -300
	let maxZ	= 300
	
	let minDelta	= -200
	let maxDelta	= 200
	
	let centers	= genPointsUniform n minZ maxZ
	let deltas	= genPointsUniformWithSeed 4321 n minDelta maxDelta

	let makePoint n (cX, cY) (dX, dY)
			= (n, cX, cY, cX + dX, cY + dY)

	let segs	= V.zipWith3 makePoint (V.enumFromTo 0 (n - 1)) centers deltas
	
	return $ World segs
	

drawWorld :: World -> G.Picture
drawWorld world
	= G.Color G.white
	$ G.Pictures
	$ map drawSegment
	$ V.toList 
	$ worldSegments world

	
drawSegment :: Segment -> G.Picture
drawSegment (ix, x1, y1, x2, y2)
	= G.Line [(f x1, f y1), (f x2, f y2)]
	where	f	= fromRational . toRational
	
handleInput :: G.Event -> World -> World
handleInput _ world 	= world


stepWorld :: Float -> World -> World
stepWorld f world	= world
