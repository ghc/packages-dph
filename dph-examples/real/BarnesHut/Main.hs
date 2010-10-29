{-# LANGUAGE ParallelListComp #-}

import World
import Body
import Util
import Timing
import MainArgs
import Config
import Points2D.Generate
import Graphics.Gloss
import System.Environment
import System.Console.ParseArgs
import Control.Monad
import Data.Maybe

import qualified Solver.ListBH.Draw		as SolverLB
import qualified Solver.ListBH.Solver		as SolverLB

import qualified Data.Vector.Unboxed		as V
import qualified Solver.VectorBH.Solver		as SolverVB
import qualified Solver.VectorNaive.Solver	as SolverVN

import qualified Data.Array.Parallel.Prelude	as P
import qualified Data.Array.Parallel.PArray	as P
import qualified Solver.NestedBH.Solver		as SolverNB

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
					, "choose one of "  ++ (show $ map fst algorithms) ])
			$ lookup solverName algorithms

	-- Make functions we'll pass to gloss simulate
	advance		= advanceWorld 
				(calcAccels $ configEpsilon config) 
				(configTimeStep config)
				(configMaxSteps config)

	draw		= drawWorld
				(configShouldDrawTree config)

	windowSize	= configWindowSize config

    in	simulateInWindow
		"Barnes-Hutt"			-- window name
		(windowSize, windowSize)	-- window size
		(10, 10)			-- window position
		black				-- background color
		(configRate config)		-- number of iterations per second
		world				-- initial world
		draw				-- fn to convert a world to a picture
		advance				-- fn to advance the world


-- Wrappers -------------------------------------------------------------------
algorithms
 = 	[ ("list-bh",		calcAccels_lb)
	, ("vector-naive",	calcAccels_vn)
	, ("vector-bh",		calcAccels_vb)
	, ("nested-bh",		calcAccels_nb) ]

-- | Lists + Barnes-Hut algorithm.
calcAccels_lb	:: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_lb epsilon mpts
	= V.fromList
	$ SolverLB.calcAccels epsilon
	$ V.toList mpts


-- | Vector + Naive algorithm.
calcAccels_vn	:: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_vn epsilon
	= SolverVN.calcAccels epsilon 
	

-- | Vector + Barnes-Hut algorithm.
calcAccels_vb 	:: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_vb epsilon mpts
	= SolverVB.calcAccels epsilon mpts


-- | Nested Data Parallelism + Barnes-Hut algorithm.
calcAccels_nb	:: Double -> V.Vector MassPoint -> V.Vector Accel
calcAccels_nb epsilon mpts
 = let	
	-- bounds finding isn't vectorised yet.
	(llx, lly, rux, ruy)	= SolverVB.findBounds mpts

	mpts'	= P.fromList $ V.toList mpts
	accels'	= SolverNB.calcAccelsWithBoxPA epsilon llx lly rux ruy mpts'
	
   in	V.fromList $ P.toList accels'
	