{-# LANGUAGE BangPatterns #-}

module World
	( World(..)
	, drawWorld
	, advanceWorld)
where
import Body
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Data.Vector.Unboxed		as V
import qualified Solver.ListBH.Draw		as SolverLB
import qualified Solver.ListBH.Solver		as SolverLB
import Debug.Trace

pointSize :: Float
pointSize		= 4

data World 
	= World
	{ -- | Bodies in the simulation.
	  worldBodies	:: V.Vector Body

	  -- | Number of steps taken in the simulation so far.
	, worldSteps	:: Int }


-- Drawing --------------------------------------------------------------------
drawWorld :: Bool -> World -> Picture
drawWorld shouldDrawTree world
 = let	picPoints	= Color (makeColor 1 1 1 0.4)
			$ Pictures 
			$ map drawBody
			$ V.toList 
			$ worldBodies world

   	picTree		= SolverLB.drawBHTree
			$ SolverLB.buildTree 
			$ map massPointOfBody
			$ V.toList 
			$ worldBodies world

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
	-> Int			-- ^ Maximum number of steps.
	-> ViewPort		-- ^ Current viewport in the gloss window.
	-> Float		-- ^ How much to advance the time in this simulation step.
	-> World -> World

advanceWorld calcAccels timeStep maxSteps _ time world
 = let	
	-- Calculate the accelerations on each body.
	accels	= calcAccels 
		$ V.map massPointOfBody 
		$ worldBodies world

	-- Apply the accelerations to the bodies and advance them.
	bodies'	= V.zipWith 
		(\body (ax, ay) 
			-> advanceBody timeStep
				(setAccelOfBody (-ax, -ay) body))
		(worldBodies world)
		accels

	-- Update the world.
	steps'	= worldSteps world + 1
	world'	= world	{ worldBodies	= bodies'
			, worldSteps	= steps' }

	-- If we have done enough steps then bail out now.
   in 	if steps' > maxSteps
	 then advanceWorld_done world
	 else world'


advanceWorld_done world
 = error "done"

