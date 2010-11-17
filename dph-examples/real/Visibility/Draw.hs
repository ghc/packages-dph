
module Draw
	( drawState
	, drawWorld)
where
import State
import World
import qualified Graphics.Gloss		as G
import qualified Data.Vector.Unboxed	as V

dtof :: Double -> Float
dtof	= fromRational . toRational

drawState :: State -> G.Picture
drawState state
 = let	picWorld	= drawWorld $ stateWorld state

	(px, py)	= stateViewPos state
	picDude		= G.Color G.green
			$ G.Translate (dtof px) (dtof py) 
			$ G.ThickCircle 2 4

   in	G.Pictures [picWorld, picDude]
	

drawWorld :: World -> G.Picture
drawWorld world
	= G.Color G.white
	$ G.Pictures
	$ map drawSegment
	$ V.toList 
	$ worldSegments world

	
drawSegment :: Segment -> G.Picture
drawSegment (_, (x1, y1), (x2, y2))
	= G.Line [(f x1, f y1), (f x2, f y2)]
	where	f	= fromRational . toRational
