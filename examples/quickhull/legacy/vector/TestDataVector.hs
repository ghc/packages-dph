{-# LANGUAGE ScopedTypeVariables, BangPatterns  #-}
{-# OPTIONS -fwarn-unused-imports #-}
module TestDataVector
	( genPointsUniform
	, genPointsDisc
	, genPointsCombo)
where
import qualified Data.Vector.Unboxed.Mutable	as MV
import qualified Data.Vector.Unboxed		as V
import Data.Vector.Unboxed			(Vector)
import qualified Data.Vector.Generic		as G		
import System.Random
import Data.Word

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
	-> Vector (Double, Double)

genPointsDisc n (originX, originY) radiusMax
 = let	

{-	(genRadius, genAngle)		
		= split $ mkStdGen seed
	
	radius	= V.fromList $ take n $ randomRs (0, radiusMax) genRadius
	angle	= V.fromList $ take n $ randomRs (- pi, pi) genAngle
-}
	radius	= V.map (\x -> (fromIntegral x / 100000) * radiusMax)
		$ randomInts n 0 100000 seed

	angle	= V.map (\x -> (fromIntegral x / 100000) * (2 * pi))
		$ randomInts n 0 100000 (seed + 1)

	makeXY r a
	 	= ( originX + r * cos a
	   	  , originY + r * sin a)	

   in	V.zipWith makeXY radius angle	


-- | A point cloud with areas of high an low density
genPointsCombo 
	:: Int 			-- ^ number of points
	-> Vector (Double, Double)

genPointsCombo n
 	=  genPointsDisc (n `div` 5) (250, 250) 200
	V.++ genPointsDisc (n `div` 5) (100, 100) 80 
	V.++ genPointsDisc (n `div` 5) (150, 300) 30 
	V.++ genPointsDisc (n `div` 5) (500, 120) 30 
	V.++ genPointsDisc (n `div` 5) (300, 200) 150


-- | Use the "minimal standard" Lehmer generator to quickly generate some random
--   numbers with reasonable statistical properties. By "reasonable" we mean good
--   enough for games and test data, but not cryptography or anything where the
--   quality of the randomness really matters.
-- 
--   From "Random Number Generators: Good ones are hard to find"
--   Stephen K. Park and Keith W. Miller.
--   Communications of the ACM, Oct 1988, Volume 31, Number 10.
--
randomInts 
	:: Int 		-- Length of vector.
	-> Int 		-- Minumum value in output.
	-> Int 		-- Maximum value in output.
	-> Int 		-- Random seed.	
	-> Vector Int	-- Vector of random numbers.

randomInts !len !valMin' !valMax' !seed'
	
 = let	-- a magic number (don't change it)
	multiplier :: Word64
	multiplier = 16807

	-- a merzenne prime (don't change it)
	modulus	:: Word64
	modulus	= 2^31 - 1

	-- if the seed is 0 all the numbers in the sequence are also 0.
	seed	
	 | seed' == 0	= 1
	 | otherwise	= seed'

	!valMin	= fromIntegral valMin'
	!valMax	= fromIntegral valMax' + 1
	!range	= valMax - valMin

	{-# INLINE f #-}
	f x		= multiplier * x `mod` modulus
 in G.create 
     $ do	
	vec		<- MV.new len

	let go !ix !x 
	  	| ix == len	= return ()
		| otherwise
		= do	let x'	= f x
			MV.write vec ix $fromIntegral $ (x `mod` range) + valMin
			go (ix + 1) x'

	go 0 (f $ fromIntegral seed)
	return vec
	
