
module Draw
	( drawState
	, drawWorld)
where
import State
import World
import Points2D.Types
import Geometry.Intersection
import qualified Graphics.Gloss		as G
import qualified Data.Vector.Unboxed	as V
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

