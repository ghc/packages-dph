
module World
where
import Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Shapes

type World = ()

drawWorld :: World -> Picture
drawWorld _
	= Color white
	$ rectangleSolid 100 100
	
handleInput :: Event -> World -> World
handleInput _ world 	= world

stepWorld :: Float -> World -> World
stepWorld f world	= world
