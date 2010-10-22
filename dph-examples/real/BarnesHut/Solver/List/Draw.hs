
module Solver.List.Draw
	(drawBHTree)
where
import Solver.List.Solver
import Graphics.Gloss
import Graphics.Gloss.Shapes


drawBHTree :: BHTree -> Picture
drawBHTree bht
 = let	Box left down right up		= bhTreeBox bht
	[left', down', right', up']	= map realToFrac [left, down, right, up]
   	picHere				= lineLoop [(left', down'), (left', up'), (right', up'), (right', down')]
	picSubs				= map drawBHTree $ bhTreeBranch bht
   in	Pictures (picHere : picSubs)
