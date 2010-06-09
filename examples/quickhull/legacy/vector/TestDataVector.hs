module TestDataVector
	( genPointsUniform
	, genPointsDisc
	, genPointsCombo)
where

import qualified Data.Array.Parallel.Unlifted 	    as U
import qualified Data.Array.Parallel.Prelude 	    as P
import qualified Data.Array.Parallel.Prelude.Double as D
import qualified Data.Array.Parallel.PArray         as P
import Data.Array.Parallel.PArray		    (PArray)

import System.Random
import Control.Exception

-- Random points generation
-- IMPORTANT: We use the same seed with the same random generator in all
--            quickhull codes.  The asymptotic work complexity of quickhull
--            is between O (N) and O (N^2) depending on the input.
--            To compare benchmark results, they always need to use the same
--            input.
seed 		= 42742

-- | Some uniformly distributed points
genPointsUniform 
	:: Int			-- ^ number of points
	-> Double		-- ^ minimum coordinate
	-> Double		-- ^ maximum coordinate
	-> [(Double, Double)]

genPointsUniform n minXY maxXY
 = let
	pointMin	= 10
	pointMax	= 510
	gen		= mkStdGen seed
   in	toPairs $ take (2*n) $ randomRs (pointMin, pointMax) gen

toPairs []        = []
toPairs (x:y:pts) = (x, y) : toPairs pts


-- | Some points distributed as a disc
genPointsDisc 
	:: Int			-- ^ number of points
	-> (Double, Double) 	-- ^ center of disc
	-> Double 		-- ^ radius of disc
	-> [(Double, Double)]

genPointsDisc n (originX, originY) radiusMax
 = let	(genRadius, genAngle)		
		= split $ mkStdGen seed
	
	radius	= take n $ randomRs (0, radiusMax) genRadius 
	angle	= take n $ randomRs (- pi, pi) genAngle

	makeXY (r, a)	
	 	= ( originX + r * cos a
	   	  , originY + r * sin a)	

   in	map makeXY $ zip radius angle	


-- | A point cloud with areas of high an low density
genPointsCombo 
	:: Int 			-- ^ number of points
	-> [(Double, Double)]	

genPointsCombo n
 	=  genPointsDisc (n `div` 5) (250, 250) 200
	++ genPointsDisc (n `div` 5) (100, 100) 80 
	++ genPointsDisc (n `div` 5) (150, 300) 30 
	++ genPointsDisc (n `div` 5) (500, 120) 30 
	++ genPointsDisc (n `div` 5) (300, 200) 150


