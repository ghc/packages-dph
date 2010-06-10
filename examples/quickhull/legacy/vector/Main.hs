{-# LANGUAGE PatternGuards #-}

import System.Environment
import Data.Function
import qualified Data.Vector.Unboxed	as V
import Data.Vector.Unboxed		(Vector)
import Control.Monad

import qualified QuickHullVector
import qualified QuickHullIO
import qualified QuickHullSplit
import TestDataVector
import Timing
import SVG

algs = 	[ ("vector",	(\v -> return $ QuickHullVector.quickHull v))
	, ("io",	QuickHullIO.quickHull)
	, ("split",	(\v -> return $ QuickHullSplit.quickHull v)) ]

parseArgs args
	| [alg, strCount]	<- args
	, Just fun 		<- lookup alg algs
	= Just (fun, read strCount, Nothing)

	| [alg, strCount, file]	<- args
	, Just fun 		<- lookup alg algs
	= Just (fun, read strCount, Just file)

	| otherwise
	= Nothing


main :: IO ()
main
 = do	argStrs		<- getArgs
	case parseArgs argStrs of
	 Just args	-> run args
	 _ 		-> putStr $ unlines
				[ "usage: quickhull <alg> <points> [out.svg]"
				, "   algs: " ++ (show $ map fst algs) ++ "\n" ]

run (fun, pointCount, mFileSVG) 
 = do
	let vPoints	= genPointsDisc pointCount (400, 400) 350 

	-- Force points to create the input vector.
	V.force vPoints `seq` return ()

	-- Compute the convex hull.
	timeStart	<- getTime
	vHull		<- fun vPoints
	V.force vHull `seq` return ()
	timeEnd		<- getTime

	-- Print how long it took.
	print (timeEnd `minus` timeStart)

	-- Dump to .svg file if requested.
	let hull	= V.toList vHull
	maybe	(return ())
	 	(\file -> writeFile file $ makeSVG 
				(roundPoints $ V.toList vPoints)
				(roundPoints $ V.toList vHull))
		mFileSVG




