
module Common
	( epsilon
	, Accel
	, MassPoint(..)
	, accel
	
	, Body
	, massPointOfBody
	, bodySetAccel
	, bodyAdvance
	, unitBody)
where

-- | If the distance between two points is less than this number
--   we ignore the forces between them.
epsilon :: Double
epsilon = 1

-- | The velocity of a point.
type Velocity	= (Double, Double)

-- | The acceleration of a point.
type Accel	= (Double, Double)


-- MassPoint ------------------------------------------------------------------
-- | A point in 2D space with its mass.
type MassPoint	= (Double, Double, Double)


-- | Calculate the acceleration on a point due to some other point.
--   If they are closer then epsilon then return 0.
accel :: MassPoint -> MassPoint -> Accel
accel (x1, y1, _) (x2, y2, m)  
	| r < epsilon	= (0.0, 0.0) 
	| otherwise	= (aabs * dx / r , aabs * dy / r)  
        where	rsqr = (dx * dx) + (dy * dy) 
		r    = sqrt rsqr 
		dx   = x1 - x2 
		dy   = y1 - y2 
		aabs = m / rsqr 


-- Body -----------------------------------------------------------------------
-- | Bodies consist of a MassPoint, but also carry their velocity
--   and acceleration between steps of the simulation.
type Body	= (MassPoint, Velocity, Accel)

massPointOfBody :: Body -> MassPoint
massPointOfBody (mp, vel, acc)	= mp

-- | Set the acceleration to a body.
bodySetAccel :: Accel -> Body -> Body
bodySetAccel acc' (mp, vel, _)	
	= (mp, vel, acc')

	
-- | Advance a body forwards in time.
bodyAdvance :: Double -> Body -> Body
bodyAdvance time 
	( (px, py, mass) 
	, (vx, vy) 
	, acc@(ax, ay))

  =	( (px + time * vx, py + time * vy, mass)
	, (vx + time * ax, vy + time * ay)
	, acc)
	
-- | Make a body with unit mass and zero vel and acc.
unitBody :: Double -> Double -> Body
unitBody x y
	= ((x, y, 1), (0, 0), (0, 0))
			



