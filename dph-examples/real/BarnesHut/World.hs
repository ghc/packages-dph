
module World
	( World
	, drawWorld
	, advanceWorld)
where
import Body
import Naive
import Graphics.Gloss
import qualified Data.Vector.Unboxed		as V

type World 
	= V.Vector Body

-- Drawing --------------------------------------------------------------------
drawWorld :: World -> Picture
drawWorld world
	= Color (makeColor 1 1 1 0.4)
	$ Pictures 
	$ map drawBody
	$ V.toList world

drawBody :: Body -> Picture
drawBody ((x, y, _), _, _)
	= drawPoint (x, y)

drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x) (realToFrac y) 
	$ ThickCircle 2 4



-- World ----------------------------------------------------------------------
-- | Advance the world forward in time.
advanceWorld 
	:: Double	-- ^ time multiplied to make simulation go faster.
	-> ViewPort	-- ^ current viewport in the gloss window.
	-> Float	-- ^ how much to advance the time in this simulation step.
	-> World -> World

advanceWorld warp _ time world
 = let	bodies	= world

	mps	= V.map massPointOfBody bodies
	accels	= calcAccels mps
	
	time'	= realToFrac time * warp
	bodies'	= V.zipWith 
			(\body (ax, ay) 
				-> bodyAdvance time' 
					(bodySetAccel (-ax, -ay) body))
			bodies
			accels
			
   in	bodies'		

