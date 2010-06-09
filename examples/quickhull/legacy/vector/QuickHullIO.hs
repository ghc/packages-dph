{-# LANGUAGE BangPatterns, PatternGuards, RankNTypes #-}

module QuickHullIO
	(quickHull)
where
import Data.Function
import Control.Monad
import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.ST
import GHC.Conc
import Data.IORef
import Data.List
import Data.Ord
import Data.Vector.Unboxed			(Vector)
import qualified Data.Vector.Unboxed		as V
import qualified Data.Vector.Unboxed.Mutable	as MV
import qualified Data.Vector.Generic		as G

type Point	= (Double, Double)
type Line	= (Point, Point)


-- | Compute the convex hull of a vector of points.
quickHull :: Vector Point -> IO (Vector Point)
quickHull !points
  | V.length points == 0	
  = return points

  | otherwise
  = do	-- Find the left and right-most points.
	let (minx, maxx) 	= minmax points

	-- Hull points get written to the vector in this IORef.
	hullRef	<- newIORef V.empty

	-- Fork off computations to handle half of the points each.
	-- For uniformly distributed points this first iteration takes most of the time.
	parIO 	[ hsplit hullRef points minx maxx
		, hsplit hullRef points maxx minx]

	-- Grab the finished hull points.
	hull	<- readIORef hullRef

	-- We've got the hull points, but they can appear in arbitrary order.
	-- Do a rubbish via-lists merge phase so that they appear clockwise around the edge.
	-- This isn't too expensive if there aren't many points on the hull.
	let (above, below) 
		= V.unstablePartition 
			(\p -> distance minx maxx p > 0)
			hull
	
	let aboveSorted	= V.fromList $ sortBy (comparing fst) $ V.toList above
	let belowSorted	= V.fromList $ sortBy (comparing fst) $ V.toList below
	let hull' = aboveSorted V.++ V.reverse belowSorted

	return hull'
	

hsplit :: IORef (Vector Point) -> Vector Point -> Point -> Point -> IO ()
{-# INLINE hsplit #-}
hsplit hullRef !points !p1@(!p1X, !p1Y) !p2@(!p2X, !p2Y)
	-- we've found one.
	| V.length packed == 0
	= addHullPoint hullRef p1
	
	-- do the two new segments in parallel.
	| V.length packed > 10000
	= parIO
		[ hsplit hullRef packed p1 pm
		, hsplit hullRef packed pm p2 ]
		
	| otherwise
	= do	hsplit hullRef packed p1 pm
		hsplit hullRef packed pm p2

	where	(packed, pm)	= parPackPoints points p1X p1Y p2X p2Y
	

-- | Copy points from the input vector that are on the left of the line into a
--	new buffer. While we're doing this, determine the point that is furthest
--	from the line.
--
--	If we have a big enough vector then split it in two and do both halves
--	in parallel. Doing this requires a copy afterwards to join the two
--	results back together. It's a trade off between decreased FP load and 
--	increased memory traffic. 
--
parPackPoints 
	:: Vector Point 
	-> Double -> Double
	-> Double -> Double
	-> ( Vector Point
	   , Point)
	
parPackPoints !points !p1X !p1Y !p2X !p2Y
 | V.length points < 1000
 = packPoints points p1X p1Y p2X p2Y

 | otherwise
 = let	len		= V.length points
	half		= len `div` 2
	rest		= len - half

	(packed1, pm1)	= packPoints (V.unsafeSlice 0 half    points) p1X p1Y p2X p2Y
	(packed2, pm2)	= packPoints (V.unsafeSlice half rest points) p1X p1Y p2X p2Y

   in	packed1 `par` packed2 `pseq`
	 let
		pMax
	 	 | V.length packed1 == 0	= pm2
	 	 | V.length packed2 == 0	= pm1
	 	 | otherwise
	 	 = let	dist1		= distance (p1X, p1Y) (p2X, p2Y) pm1
			dist2		= distance (p1X, p1Y) (p2X, p2Y) pm2
	   	   in	if dist1 > dist2
				then pm1
				else pm2

   	 in	( packed1 V.++ packed2
		, pMax)

packPoints 
	:: Vector Point 		-- Source points.
	-> Double -> Double 		-- First point on dividing line.
	-> Double -> Double 		-- Second point on dividing line.
	-> ( Vector Point		-- Packed vector containing only points on the left of the line.
	   , Point)			-- The point on the left that was furthest from the line.

{-# INLINE packPoints #-}
packPoints !points !p1X !p1Y !p2X !p2Y
 = let
	result	
	 = G.create 
 	 $ do	packed		 <- MV.new (V.length points + 1)
		(pMax, ixPacked) <- fill points packed p1X p1Y p2X p2Y 0 0

		-- We stash the maximum point on the end of the vector to get
		-- it through the create call.
		MV.unsafeWrite packed ixPacked pMax
		return $ MV.unsafeSlice 0 (ixPacked + 1) packed
	
   in	( V.unsafeSlice 0 (V.length result - 1) result
	, result `V.unsafeIndex` (V.length result - 1))
			

fill 	:: forall s
	.  Vector Point 		-- Source points.
	-> MV.MVector s Point 		-- Vector to write packed points into.
	-> Double -> Double 		-- First point on dividing line.
	-> Double -> Double		-- Second poitn on dividing line.
	-> Int 				-- Index into source points to start reading from.
	-> Int				-- Index into packed points to start writing to.
	-> ST s 
		( Point			-- Furthest point from the line that was found.
		, Int)			-- The number of packed points written.

fill !points !packed !p1X !p1Y !p2X !p2Y !ixPoints' !ixPacked'
 = go (0, 0) 0 ixPoints' ixPacked'
 where go pMax distMax ixPoints ixPacked
	| ixPoints >= V.length points	
	= do	return (pMax, ixPacked)
		
	| p	<- points `V.unsafeIndex` ixPoints
	, d	<- distance (p1X, p1Y) (p2X, p2Y) p
	, d > 0
	= do	MV.unsafeWrite packed ixPacked p
		if d > distMax
		 then	go p    d       (ixPoints + 1) (ixPacked + 1)
		 else	go pMax distMax (ixPoints + 1) (ixPacked + 1)
			
	| otherwise
	= go pMax distMax (ixPoints + 1) ixPacked


minmax :: Vector Point -> (Point, Point)
{-# INLINE minmax #-}
minmax !vec
 = go first first 0
 where	first	= vec V.! 0

	go pMin@(!minX, !minY) pMax@(!maxX, !maxY) !ix
	  | ix >= V.length vec	= (pMin, pMax)

	  | (x, y)	<- vec `V.unsafeIndex` ix
	  = if       x < minX then go (x, y) pMax   (ix + 1)
	    else if  x > maxX then go pMin   (x, y) (ix + 1)
	    else go pMin pMax (ix + 1)
	

distance :: Point -> Point -> Point -> Double
{-# INLINE distance #-}
distance (x1, y1) (x2, y2) (xo, yo)
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)


addHullPoint :: IORef (Vector Point) -> Point -> IO ()
addHullPoint hullRef p
 = atomicModifyIORef hullRef
 $ \hull -> (V.singleton p V.++ hull, ())


parIO :: [IO ()] -> IO ()
parIO stuff
 = do	mVars	<- replicateM (length stuff) newEmptyMVar
	zipWithM_ (\c v -> forkIO $ c `finally` putMVar v ()) stuff mVars
	mapM_ readMVar mVars
	

