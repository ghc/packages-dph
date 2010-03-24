
-- | Writing out matricies as PPM image files.
module PPM
	( writeMatrixAsNormalisedPPM
	, writeMatrixAsPPM 
	, rampColorHotToCold )
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Data.List				as L
import Array					as A
import Prelude					as P
import System.IO

-- | Convert a matrix to a PPM image,
--	while normalising it to the maximum value present in the matrix.
writeMatrixAsNormalisedPPM 
	:: (RealFrac a, Floating a, Ord a, Show a, U.Elt a)
	=> FilePath			-- ^ Filename
	-> (a -> (a, a, a))		-- ^ Function for producing color ramp.
	                                --   in-outputs normalised to [0..1].
	-> Array DIM2 a			-- ^ Matrix of values (need not be normalised).
	-> IO ()			-- ^ String representation of PPM image.

writeMatrixAsNormalisedPPM fileName colorFn arr
 = let	-- Use the maximum elem in the array as the white value.
	vals	= U.toList $ fromArray arr
	maxVal	= maximum vals

	-- Normalise the array to the range [0..1] for display.
	arrNorm	= A.map (/ maxVal) arr

  in	writeMatrixAsPPM fileName colorFn arrNorm


-- | Convert a matrix to a PPM image.
--	Matrix elements should be normalised to [0..1]
writeMatrixAsPPM 
	:: (RealFrac a, Floating a, Ord a, Show a, U.Elt a)
	=> FilePath
	-> (a -> (a, a, a))		-- ^ Function for producing color ramp.
					--   in-outputs normalised to [0..1]
	-> Array DIM2 a 		-- ^ Matrix of values, normalised to [0..1]
	-> IO ()			-- ^ String representation of PPM image.

writeMatrixAsPPM fileName colorFn arr
 = let		
	-- Break flat array data into individual rows
	() :*: width :*: height	 
		= arrayShape arr

	-- PPM header for pixmap image
	header	= "P3"

	prettyRGB (r, g, b)	
		=  show (truncate (r * 255)) 
		++ " " 
		++ show (truncate (g * 255)) 
		++ " " 
		++ show (truncate (b * 255))
		++ "\n"

   in do
	file	<- openFile "out.ppm" WriteMode
	hPutStrLn file $ "P3"
	hPutStrLn file $ show width ++ " " ++ show height
	hPutStrLn file $ "255"

	mapM_ (hPutStr file)
		$ L.map (prettyRGB . colorFn) 
		$ U.toList
		$ fromArray arr
		
	hClose file


-- | Break flat list into rows of a given width.
takeRows :: Int -> [a] -> [[a]]
takeRows width []	= []
takeRows width xx	= take width xx : takeRows width (drop width xx)

	
-- | Pad a string into a right justified column of a given width.
padR :: Int -> String -> String
padR width str	= L.replicate (width - length str) ' ' ++ str


-- Color Ramps  -----------------------------------------------------------------------------------
-- | Standard Hot -> Cold hypsometric color ramp.
--	Sequence is red, yellow, green, cyan, blue.
rampColorHotToCold 
	:: (Ord a, Floating a) 
	=> a 
	-> a 
	-> a 
	-> (a, a, a)
	
rampColorHotToCold vmin vmax vNotNorm
 = let	
	v	| vNotNorm < vmin	= vmin
	 	| vNotNorm > vmax	= vmax
		| otherwise		= vNotNorm
	
	dv	= vmax - vmin	

	result	| v < vmin + 0.25 * dv
		= ( 0
		  , 4 * (v - vmin) / dv
		  , 1.0)
		
		| v < vmin + 0.5 * dv
		= ( 0
		  , 1.0
		  , 1 + 4 * (vmin + 0.25 * dv - v) / dv)
		
		| v < vmin + 0.75 * dv
		= ( 4 * (v - vmin - 0.5 * dv) / dv
		  , 1.0
		  , 0.0)
		
		| otherwise
		= ( 1.0
		  , 1 + 4 * (vmin + 0.75 * dv - v) / dv
		  , 0)
		
  in	result


