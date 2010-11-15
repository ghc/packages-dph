{-# LANGUAGE BangPatterns #-}

module Common.World
	( World(..)
	, advanceWorld)
where
import Common.Body
import qualified Data.Vector.Unboxed		as V
import System.IO.Unsafe


data World 
	= World
	{ -- | Bodies in the simulation.
	  worldBodies	:: V.Vector Body

	  -- | Number of steps taken in the simulation so far.
	, worldSteps	:: Int }


-- | Advance the world forward in time.
advanceWorld 
	:: (V.Vector MassPoint	-> V.Vector Accel)
				-- ^ Fn to compute accelerations of each point.
	-> (World -> IO ())	-- ^ Fn to (unsafely) call when we've reached maxsteps.
	-> Double		-- ^ Time step.
	-> Maybe Int		-- ^ Maximum number of steps. If Nothing then run forever.
	-> Float		-- ^ How much to advance the time in this simulation step.
	-> World -> World

advanceWorld calcAccels endProgram timeStep mMaxSteps time world
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
	-- steps == 0 means keep running forever.
   in 	case mMaxSteps of
	 Nothing		-> world'
	 Just maxSteps
	  | steps' < maxSteps	-> world'
	  
	  -- just watch me..
	  | otherwise	
	  -> unsafePerformIO (endProgram world') 
		`seq` error "advanceWorld: we're finished, stop calling me."
