
module Interface
	( handleInput
	, stepState)
where
import State
import qualified Graphics.Gloss.Game	as G


-- | Handle an input event.
handleInput :: G.Event -> State -> State
handleInput (G.EventKey key keyState _ (x, y)) state
	| G.MouseButton G.LeftButton	<- key
	, G.Down			<- keyState
	= state	{ stateMode	= ModeMove 
		, stateViewPos
			= ( fromRational $ toRational x
			  , fromRational $ toRational y) }

	| G.MouseButton G.LeftButton	<- key
	, G.Up				<- keyState
	= state	{ stateMode	= ModeIdle }

handleInput (G.EventMotion (x, y)) state
	| stateMode state == ModeMove
	= state { stateViewPos
			= ( fromRational $ toRational x
			  , fromRational $ toRational y) }

handleInput _ state
	= state


stepState :: Float -> State -> State
stepState _ state = state
