
{-
	let (xs, ys)	= unzip $ P.toList vPoints
	let masses	= take pointCount $ repeat 1

	let pointMasses	 = [(x, y, m)   | x <- xs | y <- ys | m <- masses]
	let pointMasses' = [L.MP x y m  | (x, y, m) <- pointMasses]
	print pointMasses

	let tree	= L.buildTree (L.Box llX llY urX urY) pointMasses'
	print tree

	-- Compute the acceleration on each point.
	(accels, tElapsed)
		<- time 
		$  let 	accels	= L.oneStep boxLowerLeft boxUpperRight pointMasses
		   in	accels `seq` return accels
					
	-- Print how long it took.
	putStr $ prettyTime tElapsed
	
	print accels
	
	-- If we were asked for an SVG then write it out to file.
	maybe 	(return ())
	 	(\fileSVG -> 
			writeFile fileSVG
			 $ makeSVG 	(roundPoints $ P.toList vPoints) 
					(roundPoints $ P.toList vHull))
		mFileSVG
-}

