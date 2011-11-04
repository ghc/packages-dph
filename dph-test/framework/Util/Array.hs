
-- | Generic array class, used for testing only.
module Util.Array where
import Control.Monad
import Data.Vector              (Vector)
import qualified Data.Vector    as V
import qualified Prelude        as P
import Prelude                  hiding (length)


class Array a e where
 length     :: a e -> Int
 index      :: a e -> Int -> e
 append     :: a e -> a e -> a e
 toVector   :: a e -> Vector e
 fromVector :: Vector e -> a e
 

instance Array [] e where
 length         = P.length
 index          = (P.!!)
 append         = (P.++)
 toVector       = V.fromList
 fromVector     = V.toList
 

instance Array Vector e where
 length         = V.length
 index          = (V.!)
 append         = (V.++)
 toVector       = id
 fromVector     = id


-- | Convert a list to an array.
fromList :: Array a e => [e] -> a e
fromList = fromVector . V.fromList


-- | Convert an array to a list.
toList   :: Array a e => a e -> [e]
toList   = V.toList . toVector


-- | Convert the outer level of an array to vectors.
toVectors1 
        :: Array a e
        => a e -> Vector e

toVectors1 arr
        = toVector arr
        
        
-- | Convert the outer two levels of an array to vectors.
toVectors2 
        :: (Array a1 (a2 e), Array a2 e)
        => a1 (a2 e) -> Vector (Vector e)

toVectors2 = V.map toVector . toVector
        

-- | Convert the outer three levels of an array to vectors.
toVectors3
        :: (Array a1 (a2 (a3 e)), Array a2 (a3 e), Array a3 e)
        => a1 (a2 (a3 e)) -> Vector (Vector (Vector e))

toVectors3 = V.map (V.map toVector) . V.map toVector . toVector 
        
