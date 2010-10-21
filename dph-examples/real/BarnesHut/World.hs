
module World
	( World
	, drawWorld
	, advanceWorld)
where
import Body
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Data.Vector.Unboxed		as V
import qualified Naive
import qualified BarnesHutList			as BHL

type World 
	= V.Vector Body

-- Drawing --------------------------------------------------------------------
drawWorld :: World -> Picture
drawWorld world
 = let	picPoints	= Color (makeColor 1 1 1 0.4)
			$ Pictures 
			$ map drawBody
			$ V.toList world

   	picTree		= drawBHTree
			$ BHL.buildTree 
			$ map massPointOfBody
			$ V.toList world

   in	Pictures 
		[ Color (makeColor 0.0 0.5 0.0 0.5) $ picTree
		, picPoints ]


drawBody :: Body -> Picture
drawBody ((x, y, _), _, _)
	= drawPoint (x, y)

drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x) (realToFrac y) 
	$ ThickCircle 2 4

drawBHTree :: BHL.BHTree -> Picture
drawBHTree bht
 = let	BHL.Box left down right up	= BHL.bhTreeBox bht
	[left', down', right', up']	= map realToFrac [left, down, right, up]
   	picHere				= lineLoop [(left', down'), (left', up'), (right', up'), (right', down')]
	picSubs				= map drawBHTree $ BHL.bhTreeBranch bht
   in	Pictures (picHere : picSubs)


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
	accels	= calcAccels_naive mps
	
	time'	= realToFrac time * warp
	bodies'	= V.zipWith 
			(\body (ax, ay) 
				-> bodyAdvance time' 
					(bodySetAccel (-ax, -ay) body))
			bodies
			accels
			
   in	bodies'		


calcAccels_naive :: V.Vector MassPoint -> V.Vector Accel
calcAccels_naive = Naive.calcAccels
	
calcAccels_bhList :: V.Vector MassPoint -> V.Vector Accel
calcAccels_bhList mpts
	= V.fromList
	$ BHL.calcAccels
	$ V.toList mpts