{-# LANGUAGE ParallelListComp #-}

import World
import Body
import Util
import Timing
import MainArgs
import System.Environment
import Points2D.Generate
import Graphics.Gloss
import System.Console.ParseArgs
import Control.Monad
import qualified Data.Vector.Unboxed		as V


main :: IO ()
main  
 = do	args	<- parseArgsIO ArgsComplete mainArgs
	
	when (gotArg args ArgHelp)
	 $ usageError args ""

	mainWithArgs args
	

mainWithArgs :: Args MainArg -> IO ()
mainWithArgs args
 = do	let Just windowSize	= getArgInt	args ArgWindowSize
	let shouldDrawTree		= gotArg  	args ArgDrawTree
	let Just timeWarp	= getArgDouble	args ArgTimeWarp
	let Just bodyCount	= getArgInt	args ArgBodyCount
	let Just epsilon	= getArgDouble	args ArgEpsilon
	let Just discSize	= getArgDouble	args ArgDiscSize
	let Just startSpeed	= getArgDouble	args ArgStartSpeed
	
	let vPoints 	= genPointsDisc bodyCount (0, 0) discSize

	let vBodies	= V.map (setStartVelOfBody startSpeed)
			$ V.map (\(x, y) -> unitBody x y) vPoints


	simulateInWindow
		"Barnes-Hutt"			-- window name
		(windowSize, windowSize)	-- window size
		(10, 10)			-- window position
		black				-- background color
		50				-- number of iterations per second
		vBodies				-- initial world
		(drawWorld shouldDrawTree)	-- fn to convert a world to a picture
		(advanceWorld epsilon (10 * timeWarp))	-- fn to advance the world



