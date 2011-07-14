{-# LANGUAGE 
        TypeFamilies, MultiParamTypeClasses,
        FlexibleContexts,
        StandaloneDeriving, UndecidableInstances #-}
        -- Undeciable instances only need for derived Show instance

module Data.Array.Parallel.PArray.PData.Base 
        ( -- * Parallel Array types.
          PArray(..)
        , lengthPA, unpackPA
          
        , PData (..), Sized, Global

          -- * Dictionaries
        , PS(..) -- Operators on Sized arrays.
        , PJ(..) -- Projection operators.
        , PE(..) -- Expansion operators.
        , PR(..) -- Operators on array representation (combination of above)
        )
where
import qualified Data.Array.Parallel.Unlifted   as U

-- PArray ---------------------------------------------------------------------
-- | A parallel array. 
--   PArrays always contain a finite (sized) number of elements, which means
--   they have a length.
data PArray a
	= PArray Int (PData Sized a)

deriving instance (Show (PData Sized a), Show a)
	=> Show (PArray a)

lengthPA :: PArray a -> Int
lengthPA (PArray n _)   = n

unpackPA :: PArray a -> PData Sized a
unpackPA (PArray _ d)   = d

-- PData ----------------------------------------------------------------------
-- | Parallel array data.
--   As opposed to finite PArrays, a PData can represent a finite or infinite
--   number of array elements, depending on the mode. The infinite case simply
--   means that all array indices map to some element value.
--   
data family PData mode a

-- | An array with a finite size.
data Sized

-- | An infinite (globalised) array, with the same value for every element.
--   These are constructed by repeating a single value.
--   Not all array operations are possible on global arrays, in particular
--   we cannot take their length, or append global arrays to others.
data Global


-- PS Dictionary (Sized) ------------------------------------------------------
-- | Contains operations on sized arrays.
--   For methods that consume source arrays, all elements from the source 
--   may be demanded, and the total work linear in the length of the
--   source and result arrays.
--   
class PS a where
  -- | Produce an empty array with size zero.
  emptyPS	:: PData Sized a

  -- | Generate an array by applying a function to every integer in an
  --   unlifted array. This is commonly used to implement lifted projections
  --   of global arrays, as we can pass a worker function that extracts 
  --   data from a physically shared source.
  constructPS   :: (Int -> a) -> U.Array Int -> PData Sized a

  -- | Append two sized arrays.
  appPS		:: PData Sized a -> PData Sized a -> PData Sized a

  -- | Convert a sized array to a list.
  fromListPS	:: [a] -> PData Sized a

  -- TODO: Shift this into the Scalar class like existing library.  
  -- | Convert an unlifted array to a PData. 
  fromUArrayPS  :: U.Elt a => U.Array a -> PData Sized a


-- PJ Dictionary (Projection) -------------------------------------------------
-- | Contains projection operators. 
--   Projection operators may consume source arrays without demanding all 
--   the elements. These operators are indexed on the mode of the source 
--   arrays, and may have different implementations depending on whether
--   the source is Sized or Global.
-- 
--   This class has a PS superclass because the instances may also use
--   operators on sized arrays.
--  
class PS a => PJ m a where
  -- | Restrict an array to be a particular size.
  --   For pre-Sized arrays, instances should simply check that the source
  --   has the required length, and `error` if it does not. For global arrays, 
  --   we take a finite slice, copying the single element.
  --   We then rely on fusion in the back-end to eliminate the copying.
  restrictPJ    :: Int -> PData m a -> PData Sized a

  -- | Lookup a single element from the source array.
  indexPJ       :: PData m a  -> Int -> a

  -- | Lifted indexing, look up each indexed element from the corresponding array. 
  --   NOTE: This needs to be in the PJ class because that's the only one stored
  --         in the closure. Only defined for the case where a = PArray b.
  indexlPJ      :: Int -> PData m a -> PData Sized Int -> a

  replicatelPJ  :: U.Segd -> PData m a -> PData Sized a


-- PE Dictionary (Expansion) --------------------------------------------------
-- | Contains expansion operators.
--   Expansion operators construct infinite arrays from finite data.
class PS a => PE a where
  -- | Produce a globally defined array with the provided element at every index.
  --   Physically, this is performed just by storing the provided element once, 
  --   and returning it for every latter indexing operation.
  repeatPE      :: a -> PData Global a


-- PR Dictionary (Representation) ---------------------------------------------
-- | Convenience class to bundle together all the primitive operators
--   that work on our representation of parallel arrays.
class (PJ Sized a, PJ Global a, PE a) => PR a

