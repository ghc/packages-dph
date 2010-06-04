{-# LANGUAGE BangPatterns, PatternGuards #-}

module QuickHull
	(quickHull)
where
	
import Data.Function
import Data.Vector.Unboxed		as V
import Data.Vector.Unboxed.Mutable	as MV
import Data.Vector.Unboxed		(Vector)

type Point	= (Double, Double)
type Line	= (Point, Point)


distance :: Line -> Point -> Double
{-# INLINE distance #-}
distance ((x1, y1), (x2, y2)) (xo, yo)
  = (x1-xo) * (y2 - yo) - (y1 - yo) * (x2 - xo)


hsplit :: Vector Point -> Line -> Vector Point
{-# NOINLINE hsplit #-}
hsplit !points !line@(p1, p2)
	| V.length packed'	== 1
	= V.singleton p1
	
	| otherwise
	= do	let pm		= packed' `V.unsafeIndex` (V.length packed' - 1)
		let packed	= V.unsafeSlice 0 (V.length packed' - 1) packed'
		hsplit packed (p1, pm) V.++ hsplit packed (pm, p2)

	where packed' = packPoints points line


packPoints :: Vector Point -> Line -> Vector Point
{-# NOINLINE packPoints #-}
packPoints !points !line@(p1, p2)
 = V.create 
 $ do	packed	<- MV.new (V.length points + 1)
	
	-- stash the furthest point on the end of the returned vector.	
	let fill !pMax !distMax !ixPoints !ixPacked
		| ixPoints >= V.length points	
		= do	MV.unsafeWrite packed ixPacked pMax
			return ixPacked

		| p	<- points `V.unsafeIndex` ixPoints
		, d	<- distance line p
		, d > 0
		= do	MV.unsafeWrite packed ixPacked p
			if d > distMax
			 then	fill p    d       (ixPoints + 1) (ixPacked + 1)
			 else	fill pMax distMax (ixPoints + 1) (ixPacked + 1)
			
		| otherwise
		= fill pMax distMax (ixPoints + 1) ixPacked
			
	count	<- fill (0, 0) 0 0 0
	return $ MV.unsafeSlice 0 (count + 1) packed


quickHull :: Vector Point -> Vector Point
quickHull !points
  	| V.length points == 0	= points

	| (minx, maxx) 		<- minmax points
	= hsplit points (minx, maxx) V.++ hsplit points (maxx, minx)


minmax :: Vector Point -> (Point, Point)
{-# NOINLINE minmax #-}
minmax !vec
 = go (vec V.! 0) (vec V.! 0) 0
 where	go pMin@(!minX, !minY) pMax@(!maxX, !maxY) !ix
	  | ix >= V.length vec	= (pMin, pMax)

	  | (x, y)	<- vec `V.unsafeIndex` ix
	  = if       x < minX then go (x, y) pMax   (ix + 1)
	    else if  x > maxX then go pMin   (x, y) (ix + 1)
	    else go pMin pMax (ix + 1)