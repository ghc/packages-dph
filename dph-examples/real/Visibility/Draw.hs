
module Draw
	( drawState
	, drawWorld
	, drawPolarWorld)
where
import State
import World
import Points2D.Types
import Geometry.Intersection
import qualified Graphics.Gloss		as G
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Data.Maybe

dtof :: Double -> Float
dtof	= fromRational . toRational

drawState :: State -> G.Picture
drawState state
 	| ModeDisplayWorld 	<- stateModeDisplay state
 	= drawWorldWithViewPos (stateViewPos state) (stateWorld state)

	| ModeDisplayNormalised <- stateModeDisplay state
	= drawWorldWithViewPos (0, 0) 
	$ normaliseWorld (stateViewPos state)
	$ stateWorld state

	| ModeDisplayPolar	<- stateModeDisplay state
	= drawPolarWorld
	$ polarOfRectWorld
	$ normaliseWorld (stateViewPos state)
	$ stateWorld state

	| otherwise
	= G.Blank
	


drawWorldWithViewPos :: Point -> World -> G.Picture
drawWorldWithViewPos (px, py) world
 = let	
	-- the world 
	picWorld	= drawWorld world

	-- view position indicator
	picDude		= G.Color G.green
			$ G.Translate (dtof px) (dtof py) 
			$ G.ThickCircle 2 4

	-- crossings
	ptCrossings
		= catMaybes
		$ [ intersectSegHorzLine p1 p2 py
				| (_, p1, p2) <- V.toList $ worldSegments world ]

	picCrossings	= G.Pictures
			$ [ G.Color G.red
				$ G.Translate (dtof x) (dtof y)
				$ G.ThickCircle 1 2 
				| (x, y)	<- ptCrossings]


   in	G.Pictures [picWorld, picDude, picCrossings]


-- | Draw a world that is in polar coordinates.
drawPolarWorld :: World -> G.Picture	
drawPolarWorld world
 = let	projectPoint   (r, a)		= (((a - pi) / pi) * 400, r - 400)
	projectSegment (n, p1, p2)	= (n, projectPoint p1, projectPoint p2)
	
	segs'		= V.map projectSegment $ worldSegments world
		
   in	drawSegments segs'


drawWorld :: World -> G.Picture
drawWorld world
	= drawSegments
	$ worldSegments world


drawSegments :: Vector Segment -> G.Picture
drawSegments segments
 	= G.Color G.white
	$ G.Pictures
	$ map drawSegment
	$ V.toList 
	$ segments


drawSegment :: Segment -> G.Picture
drawSegment (_, (x1, y1), (x2, y2))
	= G.Line [(f x1, f y1), (f x2, f y2)]
	where	f	= fromRational . toRational

