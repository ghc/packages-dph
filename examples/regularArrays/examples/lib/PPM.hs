
-- | Writing out matricies as PPM image files.
module PPM
	( matrixToNormalisedPPM
	, matrixToPPM 
	, rampColorHotToCold )
where
import qualified Data.Array.Parallel.Unlifted 	as U
import Data.Array.Parallel.Unlifted 		((:*:)(..))
import Data.List				as L
import Array					as A
import Prelude					as P


-- | Convert a matrix to a PPM image,
--	while normalising it to the maximum value present in the matrix.
matrixToNormalisedPPM 
	:: (RealFrac a, Floating a, Ord a, Show a, U.Elt a)
	=> (a -> (a, a, a))		-- ^ Function for producing color ramp.
	                                --   in-outputs normalised to [0..1].
	-> Array DIM2 a			-- ^ Matrix of values (need not be normalised).
	-> String			-- ^ String representation of PPM image.

matrixToNormalisedPPM colorFn arr
 = let	-- Use the maximum elem in the array as the white value.
	vals	= U.toList $ fromArray arr
	maxVal	= maximum vals

	-- Normalise the array to the range [0..1] for display.
	arrNorm	= A.map (/ maxVal) arr

  in	matrixToPPM colorFn arrNorm


-- | Convert a matrix to a PPM image.
--	Matrix elements should be normalised to [0..1]
matrixToPPM 
	:: (RealFrac a, Floating a, Ord a, Show a, U.Elt a)
	=> (a -> (a, a, a))		-- ^ Function for producing color ramp.
					--   in-outputs normalised to [0..1]
	-> Array DIM2 a 		-- ^ Matrix of values, normalised to [0..1]
	-> String			-- ^ String representation of PPM image.

matrixToPPM colorFn arr
 = let		
	-- Break flat array data into individual rows
	() :*: width :*: height	 
		= arrayShape arr

	rows	= reverse (takeRows width $ U.toList $ fromArray arr)

	-- PPM header for pixmap image
	header	= "P3"

	prettyRGB (r, g, b)	
		=  show (truncate (r * 255)) 
		++ " " 
		++ show (truncate (g * 255)) 
		++ " " 
		++ show (truncate (b * 255))

	prettyRow row	
		= concat 
		$ L.intersperse "\n" 
		$ L.map (prettyRGB . colorFn)
		$ row

   in	unlines
	[ header
	, show width ++ " " ++ show height
	, show 255
	, concat 
		$ L.intersperse "\n" 
		$ L.map prettyRow rows ]


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


