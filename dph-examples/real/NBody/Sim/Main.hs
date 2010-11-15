{-# LANGUAGE ParallelListComp #-}

import Sim.MainArgs
import Sim.Config
import Common.Dump
import Common.World
import Common.Body
import Common.Util
import Solver
import Timing
import Points2D.Generate
import Graphics.Gloss
import System.Environment
import System.Console.ParseArgs
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed		as V


main :: IO ()
main  
 = do	args	<- parseArgsIO ArgsComplete mainArgs
	
	when (gotArg args ArgHelp)
	 $ usageError args ""

	mainWithArgs args
	

mainWithArgs :: Args MainArg -> IO ()
mainWithArgs args
 = let	config		= loadConfig args
	
	-- Setup initial world
	vPoints 	= genPointsDisc 
				(configBodyCount config)
	 			(0, 0) 
				(configStartDiscSize config)

	vBodies		= V.map (setStartVelOfBody $ configStartSpeed config)
			$ V.map (setMassOfBody     $ configBodyMass   config)
			$ V.map (uncurry unitBody) 
			$ vPoints

	world		= World
				{ worldBodies	= vBodies
				, worldSteps	= 0 }

	-- The solver we're using to calculate the acclerations.
	solverName	= configSolverName config
	calcAccels	= fromMaybe (error $ unlines
					[ "unknown solver " ++ show solverName
					, "choose one of "  ++ (show $ map fst solvers) ])
			$ lookup solverName solvers

	-- Make functions we'll pass to gloss simulate
	advance		= advanceWorld 
				(calcAccels $ configEpsilon config) 
				(endProgram $ configDumpFinal config)
				(configTimeStep config)
				(configMaxSteps config)

	draw		= drawWorld
				(configShouldDrawTree config)

    in	case configWindowSize config of
	 Just windowSize
	  -> simulateInWindow
		"Barnes-Hutt"		-- window name
		(windowSize, windowSize)-- window size
		(10, 10)		-- window position
		black			-- background color
		(configRate config)	-- number of iterations per second
		world			-- initial world
		draw			-- fn to convert a world to a picture
		advance			-- fn to advance the world

	 Nothing -> return ()


-- | Call error to end the program.
endProgram 
	:: Maybe FilePath		-- ^ Write final bodies to this file.
	-> World			-- ^ Final world state.
	-> IO ()

endProgram mDumpFinal world
 = do
	-- Dump the final world state to file if requested.
	maybe 	(return ()) (dumpWorld world) mDumpFinal

	-- Call error to end program.
	error $ "done"

