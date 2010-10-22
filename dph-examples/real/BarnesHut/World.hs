
module World
	( World
	, drawWorld
	, advanceWorld)
where
import Body
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Data.Vector.Unboxed		as V

import qualified Solver.Naive.Solver		as Naive
import qualified Solver.List.Draw		as BHL
import qualified Solver.List.Solver		as BHL
import qualified Solver.Vector.Solver		as BHV

type World 
	= V.Vector Body

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
	:: Double	-- ^ If points are less than this value then ignore forces on them.
	-> Double	-- ^ Time multiplier to make simulation go faster.
	-> ViewPort	-- ^ Current viewport in the gloss window.
	-> Float	-- ^ How much to advance the time in this simulation step.
	-> World -> World

advanceWorld epsilon warp _ time world
 = let	bodies	= world

	mps	= V.map massPointOfBody bodies
	accels	= calcAccels_bhVector epsilon mps
	
	time'	= realToFrac time * warp
	bodies'	= V.zipWith 
			(\body (ax, ay) 
				-> advanceBody time' 
					(setAccelOfBody (-ax, -ay) body))
			bodies
			accels
			
   in	bodies'		


calcAccels_naive :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_naive epsilon
	= Naive.calcAccels epsilon 
	
calcAccels_bhList :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_bhList epsilon mpts
	= V.fromList
	$ BHL.calcAccels epsilon
	$ V.toList mpts

calcAccels_bhVector :: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_bhVector epsilon mpts
	= V.fromList
	$ BHV.calcAccels epsilon
	$ V.toList mpts
	
	
	
	