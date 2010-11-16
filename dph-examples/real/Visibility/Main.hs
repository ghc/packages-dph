
import World
import Graphics.Gloss.Game
import Graphics.Gloss
import Graphics.Gloss.Shapes


main
 = do	world	<- initialWorld
	gameInWindow
		"Visibility"
		(1000, 1000)
		(10,  10)
		black
		100
		world
		drawWorld
		handleInput
		stepWorld
		
		
		