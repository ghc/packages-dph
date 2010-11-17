
module Geometry.Segment
	( Segment
	, polarOfRectSeg)
where
import Points2D.Types

-- | A line segement in the 2D plane.
type Segment
	= (Int, (Double, Double), (Double, Double))
	

-- | Convert a segment from rectangular to polar coordinates.
polarOfRectSeg :: Segment -> Segment
polarOfRectSeg (n, p1@(x1, y1), p2@(x2, y2))
	| y2 == 0 && x2 > 0
	= if y1 >= 0
		then (n, polarOfRectPoint p1, (magPoint p2, 0))
		else (n, polarOfRectPoint p1, (magPoint p2, 2 * pi))
	
	| y1 == 0 && x1 > 0
	= if y2 >= 0
		then (n, (magPoint p1, 0),	polarOfRectPoint p2)
		else (n, (magPoint p1, 2 * pi),	polarOfRectPoint p2)
	
	| otherwise
	= (n, polarOfRectPoint p1, polarOfRectPoint p2)


-- | Convert a point from rectangular to polar coordinates.
polarOfRectPoint :: Point -> Point
polarOfRectPoint p
 = let	r	= magPoint p
	theta	= argPoint p
   in	(r, theta)


-- | Take the magnitude of a point.
magPoint :: Point -> Double
magPoint (x, y)
	= sqrt (x * x + y * y)
	

-- | Take the angle of a point, between 0 and 2*pi radians.
argPoint :: Point -> Double
argPoint (x, y)
	= normaliseAngle $ atan2 y x


-- | Normalise an angle to be between 0 and 2*pi radians
normaliseAngle :: Double -> Double
normaliseAngle f	
	| f < 0		= normaliseAngle (f + 2 * pi)
	| f > 2 * pi	= normaliseAngle (f - 2 * pi)
	| otherwise	= f