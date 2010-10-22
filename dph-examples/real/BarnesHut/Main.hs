{-# LANGUAGE ParallelListComp #-}

import World
import Body
import Util
import Timing
import System.Environment
import Points2D.Generate
import Graphics.Gloss
import qualified Data.Vector.Unboxed		as V

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [windowSize, pointCount, discSize, startVel]	
	    -> run (read windowSize) (read pointCount) (read discSize) (read startVel)
	  _ -> usage


-- | Print command line usage information.
usage :: IO ()
usage	= putStr $ unlines
	[ "Usage: barneshutt <windowSize::Int> <points::Int> <discSize::Double> <pointSpeed::Double>"
	, ""
	, " for starters try:"
	, "       barneshutt 500 100 200 0.1"
	, ""]


-- | Run the benchmark.
run 	:: Int		-- ^ Size of window.
	-> Int 		-- ^ How many points to use.
	-> Double	-- ^ Size of disc of points.
	-> Double	-- ^ Starting rotation speed of bodies.
	-> IO ()
	
run windowSize pointCount discSize startVel
 = do	
	let vPoints 	= genPointsDisc pointCount (0, 0) discSize
	let vBodies	= V.map (setStartVelOfBody startVel)
			$ V.map (\(x, y) -> unitBody x y) vPoints

	simulateInWindow
		"Barnes-Hutt"			-- window name
		(windowSize, windowSize)	-- window size
		(10, 10)			-- window position
		black				-- background color
		50				-- number of iterations per second
		vBodies				-- initial world
		drawWorld			-- fn to convert a world to a picture
		(advanceWorld 5 10)		-- fn to advance the world



