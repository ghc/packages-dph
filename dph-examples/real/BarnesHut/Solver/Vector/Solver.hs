{-# LANGUAGE BangPatterns #-}

-- | The list version of the solver also builds the bounding box at every
--   node of the tree, which is good for visualisation.
module Solver.Vector.Solver
	( MassPoint	(..)
	, BoundingBox	(..)
	, BHTree	(..)
	, calcAccels
	, buildTree)
where
import Body
import Data.Vector.Unboxed			(Vector)
import qualified Data.Vector.Unboxed		as V

eClose :: Double
eClose  = square 500

square x = x * x
	
type BoundingBox
	= (Double, Double, Double, Double)
	
-- | The Barnes-Hut tree we use to organise the points.
data BHTree
	= BHT
	{ bhTreeCenterX	:: {-# UNPACK #-} !Double
	, bhTreeCenterY	:: {-# UNPACK #-} !Double
	, bhTreeMass	:: {-# UNPACK #-} !Double
	, bhTreeBranch	:: ![BHTree] }
	deriving Show


-- | Compute the acclerations on all these points.
calcAccels :: Double -> Vector MassPoint -> Vector Accel
calcAccels epsilon mpts
 = V.map (calcAccel epsilon (buildTree mpts)) mpts


-- | Build a Barnes-Hut tree from these points.
buildTree :: Vector MassPoint -> BHTree
buildTree mpts
	= buildTreeWithBox (findBounds mpts) mpts


-- | Find the coordinates of the bounding box that contains these points.
findBounds :: Vector MassPoint -> (Double, Double, Double, Double)
{-# INLINE findBounds #-}
findBounds bounds
 = V.foldl' acc (x1, y1, x1, y1) bounds
 where
	(x1, y1, _)	= bounds V.! 0

	acc (!llx, !lly, !rux, !ruy) (x, y, _)
	 = let	!llx'	= min llx  x
		!lly'	= min lly  y
		!rux'	= max rux  x
		!ruy'	= max ruy  y
	   in	(llx', lly', rux', ruy')


-- | Given a bounding box that contains all the points, 
--   build the Barnes-Hut tree for them.
buildTreeWithBox
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> Vector MassPoint	-- ^ points in the box.
	-> BHTree

buildTreeWithBox bb mpts
  | V.length mpts <= 1		= BHT x y m []
  | otherwise			= BHT x y m subTrees
  where	(x, y, m)		= calcCentroid   mpts
    	(boxes, splitPnts)	= splitPoints bb mpts
    	subTrees		= [buildTreeWithBox bb' ps
					| (bb', ps) <- zip boxes splitPnts]

  
-- | Split massPoints according to their locations in the quadrants.
splitPoints
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> Vector MassPoint	-- ^ points in the box.
	-> ( [BoundingBox]	-- 
	   , [Vector MassPoint])

splitPoints b@(llx, lly, rux, ruy) mpts
  | noOfPoints <= 1 = ([b], [mpts])
  | otherwise         
  = unzip [ (b,p) 
		| (b,p) <- zip boxes splitPars
		, V.length p > 0]
  where
        noOfPoints	= V.length mpts

	-- The midpoint of the parent bounding box.
        (midx,  midy)	= ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 

	-- Split the parent bounding box into four quadrants.
        b1		= (llx,  lly,  midx,  midy)
        b2		= (llx,  midy, midx,  ruy)
        b3		= (midx, midy, rux,   ruy)
        b4		= (midx, lly,  rux,   midy)
        boxes		= [b1,   b2,   b3,   b4]

	-- Sort the particles into the smaller boxes.
        lls		= V.filter (inBox b1) mpts
        lus		= V.filter (inBox b2) mpts
        rus		= V.filter (inBox b3) mpts
        rls		= V.filter (inBox b4) mpts
        splitPars	= [lls, lus, rus, rls]


-- | Check if a particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
{-# INLINE inBox #-}
inBox (llx, lly, rux, ruy) (px, py, _) 
	= (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)


-- | Calculate the centroid of some points.
calcCentroid :: Vector MassPoint -> MassPoint
{-# INLINE calcCentroid #-}
calcCentroid mpts 
  = (V.sum xs / mass, V.sum ys / mass, mass)
  where	mass     = V.sum   $ V.map (\(_, _, m) -> m) mpts
	(xs, ys) = V.unzip $ V.map (\(x, y, m) -> (m * x, m * y)) mpts


-- | Calculate the accelleration of a point due to the points in the given tree.
--   If the distance between the points is less then some small number
--   we set the accel to zero to avoid the acceleration going to infinity
--   and the points escaping the simulation. 
--
--   We also use this behavior as a hacky way to discard the acceleration
--   of a point due to interaction with itself.
--
calcAccel:: Double -> BHTree -> MassPoint -> (Double, Double)
calcAccel !epsilon (BHT x y m subtrees) mpt
	| []	<- subtrees
	= accel epsilon mpt (x, y, m)
	
	| not $ isClose mpt x y
	= accel epsilon mpt (x, y, m)

	| otherwise
	= let	(xs, ys)  = unzip [ calcAccel epsilon st mpt | st <- subtrees]
	  in	(sum xs, sum ys) 


-- | If the a point is "close" to a region in the Barnes-Hut tree then we compute
--   the "real" acceleration on it due to all the points in the region, otherwise
--   we just use the centroid as an approximation of all the points in the region.
--
isClose :: MassPoint -> Double -> Double -> Bool
{-# INLINE isClose #-}
isClose (x1, y1, m) x2 y2 
	= (x1-x2) * (x1-x2) + (y1-y2) * (y1-y2) < eClose

