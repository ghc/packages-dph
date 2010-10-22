
module Solver.List.Draw
	(drawBHTree)
where
import Solver.List.Solver
import Graphics.Gloss
import Graphics.Gloss.Shapes


drawBHTree :: BHTree -> Picture
drawBHTree bht
 = drawBHTree' 0 bht

drawBHTree' depth bht
 = let	
	-- The bounding box
	Box left down right up		= bhTreeBox bht
	[left', down', right', up']	= map realToFrac [left, down, right, up]

	picCell		= lineLoop [(left', down'), (left', up'), (right', up'), (right', down')]


	-- Draw a circle with an area equal to the mass of the centroid.
	centroidX	= realToFrac $ bhTreeCenterX bht
	centroidY	= realToFrac $ bhTreeCenterY bht
	
	centroidMass	= bhTreeMass bht
	circleRadius	= realToFrac $ sqrt (centroidMass / pi)

	midX		= (left' + right') / 2
	midY		= (up'   + down')  / 2

	picCentroid	
	 | _:_	<- bhTreeBranch bht
	 , depth >= 1
	 = Color (makeColor 0.5 0.5 1.0 0.4)
		$  Pictures
			[ Line [(midX, midY), (centroidX, centroidY)]
			, Translate centroidX centroidY 
			$ ThickCircle
				(circleRadius * 4 / 2) 
				(circleRadius * 4) ]
			
	 | otherwise
	 = Blank

	-- The complete picture for this cell.
	picHere		= Pictures [picCentroid, picCell]
		
	-- Pictures of children.
	picSubs		= map (drawBHTree' (depth + 1))
			$ bhTreeBranch bht

   in	Pictures (picHere : picSubs)
