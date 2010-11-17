
-- | Game state
module State where
import World
import Points2D.Types


-- | The game state.
data State
	= State
	{ stateWorld		:: World
	, stateMode		:: Mode
	, stateViewPos		:: Point }


-- | What mode the interface is in.
data Mode	
	-- | We're not doing anything inparticular.
	= ModeIdle

	-- | We're moving the view position.
	| ModeMove
	deriving (Show, Eq)


-- | Initial game state.
initialState :: World -> State
initialState world
	= State
	{ stateWorld		= world
	, stateMode		= ModeIdle
	, stateViewPos		= (0, 0) }
		

	
	
