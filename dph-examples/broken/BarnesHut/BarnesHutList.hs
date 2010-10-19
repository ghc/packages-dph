
module BarnesHutList 
	( MassPoint	(..)
	, BoundingBox	(..)
	, BHTree	(..)
	, oneStep
	, buildTree)
where

-- | If the distance between two points is less than this number
--   we ignore the forces between them.
epsilon :: Double
epsilon = 0.05

eClose :: Double
eClose  = 0.5

-- | The acceleration of a point.
type Accel	= (Double, Double)


-- | A point in 2D space with its mass.
data MassPoint
	= MP 
	{ massPointPosX	:: Double
	, massPointPosY	:: Double
	, massPointMass	:: Double }


-- | A rectangular region in 2D space.
data BoundingBox
	= Box 
	{ boxLowerLeftX	 :: Double
	, boxLowerLeftY	 :: Double
	, boxUpperRightX :: Double
	, boxUpperRightY :: Double }


-- | The Barnes-Hut tree we use to organise the points.
data BHTree
	= BHT
	{ bhTreeCenterX	:: Double
	, bhTreeCenterY	:: Double
	, bhTreeMass	:: Double
	, bhTreeBranch	:: [BHTree] }
	deriving Show

-- | Compute a single step of the simulation.
oneStep	:: (Double, Double)	-- ^ coord of lower left  corner of bounding box
	-> (Double, Double)	-- ^ coord of upper right corner of bounding box
	-> [(Double, Double, Double)]	-- ^ (x, y, mass) of each point
	-> ([Double], [Double])	-- ^ acceleration of each point.

oneStep (llx, lly) (rux, ruy) mspnts 
 = (xs, ys)
 where	(xs, ys) = unzip [ calcAccel m tree | m <- ms ]
	tree	 = buildTree (Box llx lly rux ruy) ms
	ms	 = [ MP x y m | (x,y,m) <-  mspnts]


-- | Build a Barnes-Hut tree from these points.
buildTree
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> [MassPoint]		-- ^ points in the box.
	-> BHTree

buildTree bb particles
  | length particles <= 1 = BHT x y m []
  | otherwise             = BHT x y m subTrees
  where
    (MP x y m)           = calcCentroid particles
    (boxes, splitPnts)   = splitPoints bb particles 
    subTrees             = [buildTree bb' ps | (bb', ps) <- zip boxes splitPnts]

  
-- | Split massPoints according to their locations in the quadrants.
splitPoints
	:: BoundingBox		-- ^ bounding box containing all the points.
	-> [MassPoint]		-- ^ points in the box.
	-> ( [BoundingBox]	-- 
	   , [[MassPoint]])

splitPoints b@(Box llx lly rux  ruy) particles 
  | noOfPoints <= 1 = ([b], [particles])
  | otherwise         
  = unzip [ (b,p) | (b,p) <- zip boxes splitPars, length p > 0]
  where
        noOfPoints	= length particles

	-- The midpoint of the parent bounding box.
        (midx,  midy)	= ((llx + rux) / 2.0 , (lly + ruy) / 2.0) 

	-- Split the parent bounding box into four quadrants.
        b1		= Box llx  lly  midx midy
        b2		= Box llx  midy midx  ruy
        b3		= Box midx midy rux   ruy
        b4		= Box midx lly  rux  midy
        boxes		= [b1,   b2,  b3,  b4]

	-- Sort the particles into the smaller boxes.
        lls		= [ p | p <- particles, inBox b1 p ]
        lus		= [ p | p <- particles, inBox b2 p ]
        rus		= [ p | p <- particles, inBox b3 p ]
        rls		= [ p | p <- particles, inBox b4 p ]
        splitPars	= [lls, lus, rus, rls]


-- | Check if a particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
inBox (Box llx  lly rux  ruy) (MP px  py  _) = 
    (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)


-- | Calculate the centroid of some points.
calcCentroid :: [MassPoint] -> MassPoint
calcCentroid mpts = MP (sum xs / mass) (sum ys / mass) mass
  where
    mass     = sum   [ m | MP _ _ m  <- mpts ]
    (xs, ys) = unzip [ (m * x, m * y) | MP x y m <- mpts ]   


-- | Calculate the accelleration of a point due to the points in the given tree.
--   If the distance between the points is less then some small number
--   we set the accel to zero to avoid the acceleration going to infinity
--   and the points escaping the simulation. 
--
--   We also use this behavior as a hacky way to discard the acceleration
--   of a point due to interaction with itself.
--
calcAccel:: MassPoint -> BHTree -> (Double, Double)
calcAccel mpt (BHT x y m subtrees)
	| isClose mpt x y = accel mpt x y m
	| otherwise       = (sum xs, sum ys) 
	where	(xs, ys)  = unzip [ calcAccel mpt st | st <- subtrees]


-- | If the a point is "close" to a region in the Barnes-Hut tree then we compute
--   the "real" acceleration on it due to all the points in the region, otherwise
--   we just use the centroid as an approximation of all the points in the region.
--
--   TODO: Isn't this comparison the wrong way around??
--
isClose :: MassPoint -> Double -> Double -> Bool
isClose (MP x1 y1 m) x2 y2 
	= (x1-x2) * (x1-x2) + (y1-y2) * (y1-y2) < eClose


-- | Calculate the acceleration on a point due to some other point.
accel :: MassPoint -> Double -> Double -> Double -> Accel
accel (MP x1  y1 _) x2 y2 m  
	| r < epsilon	= (0.0, 0.0) 
	| otherwise	= (aabs * dx / r , aabs * dy / r)  
        where	rsqr = (dx * dx) + (dy * dy) 
		r    = sqrt rsqr 
		dx   = x1 - x2 
		dy   = y1 - y2 
		aabs = m / rsqr 

