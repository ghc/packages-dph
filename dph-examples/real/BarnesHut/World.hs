{-# LANGUAGE BangPatterns #-}

module World
	( World
	, drawWorld
	, advanceWorld)
where
import Body
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Data.Vector.Unboxed		as V
import qualified Solver.List.Draw		as BHL
import qualified Solver.List.Solver		as BHL
import Debug.Trace

type World = V.Vector Body

pointSize :: Float
pointSize		= 4

-- Drawing --------------------------------------------------------------------
drawWorld :: Bool -> World -> Picture
drawWorld shouldDrawTree world
 = let	picPoints	= Color (makeColor 1 1 1 0.4)
			$ Pictures 
			$ map drawBody
			$ V.toList world

   	picTree		= BHL.drawBHTree
			$ BHL.buildTree 
			$ map massPointOfBody
			$ V.toList world

   in	Pictures 
		[ if shouldDrawTree 
			then Color (makeColor 0.5 1.0 0.5 0.2) $ picTree
			else Blank
			
		, picPoints ]


drawBody :: Body -> Picture
drawBody ((x, y, _), _, _)
	= drawPoint (x, y)

drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x) (realToFrac y) 
	$ ThickCircle (pointSize / 2) pointSize



-- World ----------------------------------------------------------------------
-- | Advance the world forward in time.
advanceWorld 
	:: (V.Vector MassPoint	-> V.Vector Accel)
				-- ^ Fn to compute accelerations of each point.
	-> Double		-- ^ Time step.
	-> ViewPort		-- ^ Current viewport in the gloss window.
	-> Float		-- ^ How much to advance the time in this simulation step.
	-> World -> World

advanceWorld calcAccels timeStep _ time bodies
 = let	accels	= calcAccels 
		$ V.map massPointOfBody bodies

   in	V.zipWith 
		(\body (ax, ay) 
			-> advanceBody timeStep
				(setAccelOfBody (-ax, -ay) body))
		bodies
		accels
	