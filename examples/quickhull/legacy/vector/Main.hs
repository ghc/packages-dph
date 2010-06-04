{-# LANGUAGE PatternGuards #-}

import System.Environment
import Data.Function
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)


import QuickHull
import TestData
import Timing
import SVG

parseArgs args
	| [strCount]	<- args
	= (read strCount, Nothing)

	| [strCount, file]	<- args
	= (read strCount, Just file)


main :: IO ()
main
 = do	args		<- getArgs
	let (pointCount, mFileSVG) = parseArgs args
	
	let points	= genPointsDisc pointCount (400, 400) 350 
	let vPoints	= V.fromList points

	timeStart	<- getTime
	let vHull	= quickHull vPoints
	V.force vHull `seq` return ()
	timeEnd		<- getTime

	print (timeEnd `minus` timeStart)

	let hull	= V.toList vHull
	
	maybe	(return ())
	 	(\file -> writeFile file $ makeSVG 
				(roundPoints $ V.toList vPoints)
				(roundPoints $ V.toList vHull))
		mFileSVG




