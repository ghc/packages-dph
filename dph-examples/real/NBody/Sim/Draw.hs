
-- | Drawing the world as a gloss picture.
module Sim.Draw
	(drawWorld)
where
import Common.World
import Common.Body
import Graphics.Gloss
import Graphics.Gloss.Shapes
import qualified Solver.ListBH.Draw		as SolverLB
import qualified Solver.ListBH.Solver		as SolverLB
import qualified Data.Vector.Unboxed		as V


pointSize :: Float
pointSize	= 4


-- | Draw the world, and optionally show the Barnes-Hut tree.
drawWorld :: Bool -> World -> Picture
drawWorld shouldDrawTree world
 = let	picPoints	= Color (makeColor 1 1 1 0.4)
			$ Pictures 
			$ map drawBody
			$ V.toList 
			$ worldBodies world

   	picTree		= SolverLB.drawBHTree
			$ SolverLB.buildTree 
			$ map massPointOfBody
			$ V.toList 
			$ worldBodies world

   in	Pictures 
		[ if shouldDrawTree 
			then Color (makeColor 0.5 1.0 0.5 0.2) $ picTree
			else Blank
			
		, picPoints ]


-- | Draw a single body.
drawBody :: Body -> Picture
drawBody ((x, y, _), _, _)
	= drawPoint (x, y)


-- | Draw a point using a filled circle.
drawPoint :: (Double, Double) -> Picture
drawPoint (x, y)
	= Translate (realToFrac x) (realToFrac y) 
	$ ThickCircle (pointSize / 2) pointSize
