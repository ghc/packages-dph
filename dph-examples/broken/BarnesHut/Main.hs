{-# LANGUAGE ParallelListComp #-}

import World
import Common
import Timing
import System.Environment
import Points2D.Generate
import Graphics.Gloss
import qualified BarnesHutList			as L
import qualified Data.Vector.Unboxed		as V
import qualified Data.Array.Parallel.PArray	as P

main :: IO ()
main 
 = do	args	<- getArgs
	case args of
	  [pointCount]	
	    -> run (read pointCount) Nothing
	
	  [pointCount, fileSVG]
	    -> run (read pointCount) (Just fileSVG)

	  _ -> do
		putStr usage
		return ()


-- | Command line usage information.
usage :: String
usage	= unlines
	[ "Usage: barneshutt <points> [out.svg]"	]


-- | Run the benchmark.
run 	:: Int 			-- ^ How many points to use.
	-> Maybe String 	-- ^ File name to dump an SVG of the output to.
	-> IO ()
	
run pointCount mFileSVG
 = do
	let boxLowerLeft @(llX, llY)	= (  0,   0)
	let boxUpperRight@(urX, urY)	= (400, 400)
	
	let vPoints 	= genPointsDisc pointCount (0, 0) 400
	let vBodies	= V.map (\(x, y) -> unitBody x y) vPoints

	simulateInWindow
		"Barnes-Hutt"		-- window name
		(1000, 1000)		-- window size
		(1200, 10)		-- window position
		black			-- background color
		50			-- number of iterations per second
		vBodies			-- initial world
		drawWorld		-- fn to convert a world to a picture
		(advanceWorld 100)	-- fn to advance the world
