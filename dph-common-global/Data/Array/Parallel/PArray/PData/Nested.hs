
{-# LANGUAGE
	TypeFamilies,
	FlexibleInstances, FlexibleContexts,
	MultiParamTypeClasses,
	StandaloneDeriving,
	ExistentialQuantification,
	UndecidableInstances #-}

module Data.Array.Parallel.PArray.PData.Nested where
import Data.Array.Parallel.PArray.PData.Scalar
import Data.Array.Parallel.PArray.PData.Base

import qualified Data.Array.Parallel.Unlifted   as U
import Debug.Trace


-- PData Sized (PData m a) ----------------------------------------------------
data instance PData Sized  (PArray a)
          -- A sized array of sized data.
	= PNestedS U.Segd (PData Sized a)

data instance PData Global (PArray a)
	= PNestedG (PArray a)


instance Show (PData Sized a) => Show (PData Sized (PArray a)) where
        show (PNestedS _ d1)
                = "(PNested " ++ " (" ++ show d1 ++ "))"


instance PS a => PS (PArray a) where
  {-# INLINE_PDATA emptyPS #-}
  emptyPS
	= PNestedS (U.mkSegd U.empty U.empty 0) emptyPS


  {-# INLINE_PDATA appPS #-}
  appPS (PNestedS segd1 d1) (PNestedS segd2 d2) 
   	= error "appPS@PArray undefined"


  {-# INLINE_PDATA fromListPS #-}
  fromListPS xx
   = case xx of
      []      -> emptyPS
      xx@(x:xs)
       -> PNestedS
                (U.lengthsToSegd $ U.fromList $ map lengthPA xx)
                (foldl1 appPS $ map unpackPA xx)

        
instance PJ Sized a => PJ Sized (PArray a) where 
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n arr@(PNestedS segd d1)
   = arr


  -- Lifted replicate.
  -- logical  restrictsPJ :: [Int] -> [[a]] -> [[[a]]]
  --    eg:   restrictsPJ [2 3 1]   [  [x0 x1]           [x2]             [x3 x4 x5]]
  --                    =>          [ [[x0 x1] [x0 x1]] [[x2] [x2] [x2]] [[x3 x4 x5]]

  -- physical restrictsPJ :: U.Segd -> PData Sized (PArray a) -> PData Sized (PArray a)
  --          restrictsPJ  [2 3 1]  [ x0 x1 x2 x3 x4 x5 ]  (data)
  --                                [ 2 1 3 ]              (lengths)
  --                                [ 0 2 3 ]              (indices)

  --                    =>          [ x0 x1 x2 x3 x4 x5 ] (data)
  --                                [ 2 2 1 1 1 3 ]       (lengths)
  --                                [ 0 0 2 2 2 3 ]       (indices)
  {-# INLINE_PDATA restrictsPJ #-}
  restrictsPJ segd1 (PNestedS segd2 d2)
   = let segd'  = U.mkSegd (U.replicate_s segd1 (U.lengthsSegd segd2))
                           (U.replicate_s segd1 (U.indicesSegd segd2))
                           (error "replicatelPS@Sized PArray: no size for flat array")
     in  PNestedS segd' d2


  {-# INLINE_PDATA indexlPJ #-}
  indexlPJ n (PNestedS segd d1) arrIxs
   = let -- Ensure the indices array has the correct size.
         PIntS ixs  = restrictPJ n arrIxs
   
         -- Compute the indices of all the elements we want from the flat source array.
         ixsFlat    = U.zipWith (+) (U.indicesSegd segd) ixs
 
         -- Lookup all the elements we want from the flat array.
     in  PArray (error "indexlPJ@Sized PArray: fake segd should not be touched by caller")
          $ constructPS (indexPJ d1) ixsFlat



instance PR a => PJ Global (PArray a) where
  {-# INLINE_PDATA restrictPJ #-}
  restrictPJ n1 arr@(PNestedG (PArray n2 d2))
   = let segd'  = U.mkSegd  (U.replicate n1 n2)
                            (U.replicate n1 0)
                            (error "restrictPJ@Global PArray: no size for flat array")
     in PNestedS segd' d2


  -- When restricting a global array, all the segments in the result
  -- share the same source data.
  {-# INLINE_PDATA restrictsPJ #-}
  restrictsPJ segd1 (PNestedG (PArray n d2))
   = let segd'  = U.mkSegd  (U.replicate_s segd1 (U.replicate (U.lengthSegd segd1) n))
                            (U.replicate_s segd1 (U.replicate (U.lengthSegd segd1) 0))
                            (error "replicatelPJ@Global PArray: no size for flat array")
     in PNestedS segd' d2


  {-# INLINE_PDATA indexlPJ #-}
  indexlPJ n (PNestedG (PArray _ d1)) arrIxs
   = let -- Ensure the indices array has the correct size.
         PIntS ixs  = restrictPJ n arrIxs

         -- When indexing into a global nested array, all the elements of the source
         -- are identical so we can use the original indices without offsetting them.
     in  PArray (error "indexlPJ@Global PArray: fake segd should not be touched by caller")
          $ constructPS (indexPJ d1) ixs


instance PE a => PE (PArray a) where
  {-# INLINE_PDATA repeatPE #-}
  repeatPE x = PNestedG x


instance PR a => PR (PArray a)

