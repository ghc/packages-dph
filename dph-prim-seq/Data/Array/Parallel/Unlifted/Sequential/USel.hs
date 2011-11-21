-- | A selector is a description of how to perform a `combine` operation.
--
-- Suppose we are evaluating the following expression:
--
--   @combine [F,F,T,F,T,T] [1,2,3] [4,5,6] = [4,5,1,6,2,3]@
--
-- This is difficult to parallelise. For each element in the result, the source 
-- array we get this element from depends on the tag values associated with 
-- all previous elements.
--
-- However, if we going to perform several combines with the same tag array, we
-- can precompute a selector that tells us where to get each element. The selector
-- contains the original tags, as well as the source index telling us where to get
-- each element for the result array.
--
-- For example:
--
--  @
--  tagsToIndices2 [F,F,T,F,T,T]   -- tags
--               = [0,1,0,2,1,2]   -- indices
--  @
--
--  This says get the first element from index 0 in the second array, then from index 1 in the second array,
--  then index 0 in the first array ...
--  
--  The selector then consists of both the @tag@ and @indices@ arrays.
--
{-# LANGUAGE CPP #-}

#include "fusion-phases.h"

module Data.Array.Parallel.Unlifted.Sequential.USel (
  -- * Types
  USel2,

  -- * Operations on selectors
  mkUSel2,
  lengthUSel2,
  tagsUSel2, indicesUSel2, elementsUSel2_0, elementsUSel2_1,
  tagsToIndices2
) where
import Data.Array.Parallel.Unlifted.Sequential.Vector as V
import qualified Data.Vector.Fusion.Stream as S
import Data.Vector.Fusion.Stream.Monadic ( Stream(..) )
import Data.Array.Parallel.Base (Tag)


-- | Abstract selector. 
--   Contains both the @tags@ and @indices@ arrays outlined above.
data USel2 
        = USel2
        { usel2_tags      :: !(Vector Tag)
        , usel2_indices   :: !(Vector Int)
        , usel2_elements0 :: !Int               -- ^ Number of tags with value 0.
        , usel2_elements1 :: !Int               -- ^ Number of tags with value 1.
        }


-- | O(1). Construct a selector.
mkUSel2 :: Vector Tag           -- ^ Tags array.
        -> Vector Int           -- ^ Indices array
        -> Int                  -- ^ Number of elements taken from first array.
        -> Int                  -- ^ Number of elements taken from second array.
        -> USel2
mkUSel2 = USel2
{-# INLINE mkUSel2 #-}


-- Projections ----------------------------------------------------------------
-- INLINE trivial projections as they'll expand to a single record selector.

-- | O(1). Get the number of elements represented by this selector.
--         This is the length of the array returned by `combine`.
lengthUSel2 :: USel2 -> Int
lengthUSel2     = V.length . usel2_tags
{-# INLINE lengthUSel2 #-}


-- | O(1). Get the tags array of a selector.
tagsUSel2 :: USel2 -> Vector Tag
{-# INLINE tagsUSel2 #-}
tagsUSel2       = usel2_tags


-- | O(1). Get the indices array of a selector.
indicesUSel2 :: USel2 -> Vector Int
indicesUSel2    = usel2_indices
{-# INLINE indicesUSel2 #-}


-- | O(1). Get the number of elements that will be taken from the first array.
elementsUSel2_0 :: USel2 -> Int
elementsUSel2_0 = usel2_elements0
{-# INLINE elementsUSel2_0 #-}


-- | O(1). Get the number of elements that will be taken from the second array.
elementsUSel2_1 :: USel2 -> Int
elementsUSel2_1 = usel2_elements1
{-# INLINE elementsUSel2_1 #-}


-- | O(n). Compute the source index for each element of the result array.
tagsToIndices2 :: Vector Tag -> Vector Int
tagsToIndices2 tags 
  = unstream (mapAccumS add (0,0) (stream tags))
  where
    add (i,j) 0 = ((i+1,j),i)
    add (i,j) _ = ((i,j+1),j)
{-# INLINE_STREAM tagsToIndices2 #-}


mapAccumS :: (acc -> a -> (acc,b)) -> acc -> S.Stream a -> S.Stream b
mapAccumS f acc (Stream step s n)
  = Stream step' (acc,s) n
  where
   {-# INLINE_INNER step' #-}
   step' (acc,s) 
    = do r <- step s
         case r of
          S.Yield x s' -> let (acc',y) = f acc x
                          in return $ S.Yield y (acc',s')
          S.Skip    s' -> return $ S.Skip (acc,s')
          S.Done       -> return S.Done
{-# INLINE_STREAM mapAccumS #-}
