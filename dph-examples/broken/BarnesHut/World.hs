
module World
	( World
	, drawWorld
	, advanceWorld)
where
import Common
import Naive
import Graphics.Gloss
import qualified Data.Vector.Unboxed		as V


type World	
	= V.Vector Body

-- Drawing --------------------------------------------------------------------
drawWorld :: World -> Picture
drawWorld world
	= Color (makeColor 1 1 1 0.8)
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
advanceWorld :: Double -> ViewPort -> Float -> World -> World
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

