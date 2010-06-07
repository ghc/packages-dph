{-# LANGUAGE CPP, FlexibleContexts #-}

#include "fusion-phases.h"

-- |Underlying data types and type classes for parallel arrays.
--
--	For motivational material see:
--	   "An Approach to Fast Arrays in Haskell", Chakravarty and Keller, 2003
--
--	For discussion of how the mapping to generic types works see:
--         "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
--
module Data.Array.Parallel.Lifted.PArray (
  PArray(..), PData,

  PA(..),
  lengthPA#, dataPA#, replicatePA#, replicatelPA#, repeatPA#,
  emptyPA, indexPA#, extractPA#, bpermutePA#, appPA#, applPA#,
  packByTagPA#, combine2PA#, updatePA#, fromListPA#, fromListPA, nfPA,

  replicatePD, replicatelPD, repeatPD, emptyPD,
  indexPD, extractPD, bpermutePD, appPD, applPD,
  packByTagPD, combine2PD, updatePD, fromListPD, nfPD,

  PRepr, PR(..),

  Scalar(..),
  replicatePRScalar, replicatelPRScalar, repeatPRScalar, emptyPRScalar,
  indexPRScalar, extractPRScalar, bpermutePRScalar, appPRScalar, applPRScalar,
  packByTagPRScalar, combine2PRScalar, updatePRScalar, fromListPRScalar,
  nfPRScalar,
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.Unboxed ( elementsSegd# )
import Data.Array.Parallel.Base ( traceF )
import GHC.Exts (Int#, Int(..), (+#), (*#))
import SpecConstr


-- |Lifted\/bulk parallel arrays
--   A parallel array with elements of type a.
--
{-# ANN type PArray NoSpecConstr #-}
data PArray a = PArray Int# (PData a)


-- |Parallel array data.
--
--   The family of types that store parallel array data.
--   PData takes the type of an element and produces the type we use to store
--   an array of those elements. 
--
--   For example, for performance reasons we store an array of pairs of ints
--   as a pair of two arrays of ints.
--
--   Instances for simple types are defined in Data.Array.Parallel.Lifted.Repr 
--
{-# ANN type PData NoSpecConstr #-}
data family PData a


-- |Representable types.
--
--   The family of types that we know how to represent generically.
--   PRepr takes an arbitrary type and produces the generic type we use to 
--   represent it.
--
--   Instances for simple types are defined in Data.Array.Parallel.Lifted.Instances.
--   For algebraic types, it's up to the client module to provide a suitable instance.
--
type family PRepr a


-- Dictionaries -----------------------------------------------------------------------------------

-- |A PA dictionary contains the functions that we use to convert a
--   representable type to its representation, and back.
--
class PR (PRepr a) => PA a where
  toPRepr      :: a -> PRepr a
  fromPRepr    :: PRepr a -> a
  toArrPRepr   :: PData a -> PData (PRepr a)
  fromArrPRepr :: PData (PRepr a) -> PData a


-- |A PR dictionary contains the primitive functions that operate directly
--    on parallel array data.
-- 
--    It's called PR because the functions work on our internal Representation
--    of the user-level array.
--
class PR a where
  emptyPR      :: T_emptyPR a
  replicatePR  :: T_replicatePR a
  replicatelPR :: T_replicatelPR a
  repeatPR     :: T_repeatPR a
  indexPR      :: T_indexPR a
  extractPR    :: T_extractPR a
  bpermutePR   :: T_bpermutePR a
  appPR        :: T_appPR a
  applPR       :: T_applPR a
  packByTagPR  :: T_packByTagPR a
  combine2PR   :: T_combine2PR a
  updatePR     :: T_updatePR a
  fromListPR   :: T_fromListPR a
  nfPR         :: T_nfPR a


-- |An empty array.
type T_emptyPR      a =  PData a


-- |Produce an array containing copies of a given element.
type T_replicatePR  a =  Int#              -- number of copies \/ elements in resulting array
                      -> a                 -- element to replicate
                      -> PData a


type T_replicatelPR a =  U.Segd            -- segment descriptor of result array
                      -> PData a 
                      -> PData a


-- |Produce an array containing copies of some other array.
type T_repeatPR     a =  Int#              -- number of times to repeat
                      -> Int#              -- length of source array
                      -> PData a           -- source array
                      -> PData a

-- |Retrieve a numbered element from an array.
type T_indexPR      a =  PData a           -- source array
                      -> Int#              -- index of desired element
                      -> a


-- |Extract a subrange of elements from an array.
--   eg extract [:23, 42, 93, 50, 27:] 1 3  = [:42, 93, 50:]
-- 
type T_extractPR    a =  PData a           -- source array
                      -> Int#              -- starting index
                      -> Int#              -- length of result array
                      -> PData a


-- |Construct a new array by selecting elements from a source array.
--   eg  bpermute [:50, 60, 20, 30:] 3 [:0, 3, 2:]  = [:50, 30, 20:]
--
type T_bpermutePR   a =  PData a           -- source array
                      -> Int#              -- length of resulting array
                      -> U.Array Int       -- indices of elements in source array
                      -> PData a          


-- |Append two arrays.
type T_appPR        a = PData a -> PData a -> PData a

type T_applPR       a =  U.Segd              -- result segd
                      -> U.Segd -> PData a   -- src segd/data 1
                      -> U.Segd -> PData a   -- src segd/data 2
                      -> PData a


-- |Select some elements from an array that correspond to a particular tag value
--	and pack them into a new array.
--   eg  packByTag [:23, 42, 95, 50, 27, 49:]  3 [:1, 2, 1, 2, 3, 2:] 2 = [:42, 50, 49:]
--
type T_packByTagPR  a = PData a            -- source array
                      -> Int#              -- length of resulting array
                      -> U.Array Int       -- tag values of elements in source array
                      -> Int#              -- tag value of the elements to select
                      -> PData a


-- |Combine two arrays based on a selector
--     The selector says which source array to choose for each element of the
--     resulting array.
type T_combine2PR   a =  Int#              -- length of resulting array
                      -> U.Sel2            -- selector
                      -> PData a           -- first source array
                      -> PData a           -- second source array
                      -> PData a

type T_updatePR     a =  PData a
                      -> U.Array Int
                      -> PData a
                      -> PData a


-- |Convert a list to an array.
type T_fromListPR a = Int#                 -- length of resulting array
                    -> [a]                 -- source list
                    -> PData a


-- |Force an array to normal form.
type T_nfPR a = PData a -> ()


-- PD Wrappers ------------------------------------------------------------------------------------
--
--   Given some data that has a PA dictionary, we can convert it to its representation
--   type, perform the requested operation, then convert it back.
--
emptyPD :: PA a => T_emptyPR a
{-# INLINE_PA emptyPD #-}
emptyPD = fromArrPRepr emptyPR

replicatePD :: PA a => T_replicatePR a
{-# INLINE_PA replicatePD #-}
replicatePD n# x = fromArrPRepr
                 . replicatePR n#
                 $ toPRepr x

replicatelPD :: PA a => T_replicatelPR a
{-# INLINE_PA replicatelPD #-}
replicatelPD segd xs = fromArrPRepr
                     . replicatelPR segd
                     $ toArrPRepr xs
    
repeatPD :: PA a => T_repeatPR a
{-# INLINE_PA repeatPD #-}
repeatPD n# len# xs = fromArrPRepr
                    . repeatPR n# len#
                    $ toArrPRepr xs

indexPD :: PA a => T_indexPR a
{-# INLINE_PA indexPD #-}
indexPD xs i# = fromPRepr $ indexPR (toArrPRepr xs) i#

extractPD :: PA a => T_extractPR a
{-# INLINE_PA extractPD #-}
extractPD xs i# m# = fromArrPRepr $ extractPR (toArrPRepr xs) i# m#

bpermutePD :: PA a => T_bpermutePR a
{-# INLINE bpermutePD #-}
bpermutePD xs n# is = fromArrPRepr $ bpermutePR (toArrPRepr xs) n# is

appPD :: PA a => T_appPR a
{-# INLINE_PA appPD #-}
appPD xs ys = fromArrPRepr $ appPR (toArrPRepr xs) (toArrPRepr ys)

applPD :: PA a => T_applPR a
{-# INLINE_PA applPD #-}
applPD segd is xs js ys
  = fromArrPRepr $ applPR segd is (toArrPRepr xs) js (toArrPRepr ys)

packByTagPD :: PA a => T_packByTagPR a
{-# INLINE_PA packByTagPD #-}
packByTagPD xs n# tags t#
  = fromArrPRepr $ packByTagPR (toArrPRepr xs) n# tags t#

combine2PD :: PA a => T_combine2PR a
{-# INLINE_PA combine2PD #-}
combine2PD n# sel as bs
  = fromArrPRepr $ combine2PR n# sel (toArrPRepr as) (toArrPRepr bs)

updatePD :: PA a => T_updatePR a
{-# INLINE_PA updatePD #-}
updatePD xs is ys
  = fromArrPRepr $ updatePR (toArrPRepr xs) is (toArrPRepr ys)

fromListPD :: PA a => T_fromListPR a
{-# INLINE_PA fromListPD #-}
fromListPD n# xs = fromArrPRepr $ fromListPR n# (map toPRepr xs)

nfPD :: PA a => T_nfPR a
{-# INLINE nfPD #-}
nfPD xs = nfPR (toArrPRepr xs)


-- PA Wrappers ------------------------------------------------------------------------------------
--
-- These functions operate on whole PArrays.
--   For most of these we can just take the parallel array data (PData) from the 
--   array structure then apply the corresponding PD wrapper. Depending on
--   the function we may also need to calculuate the length of the resulting array.
--

-- Simple projections
-- | Take the length field of a PArray.
lengthPA# :: PArray a -> Int#
{-# INLINE_PA lengthPA# #-}
lengthPA# (PArray n# _) = n#

-- | Take the data field of a PArray.
dataPA# :: PArray a -> PData a
{-# INLINE_PA dataPA# #-}
dataPA# (PArray _ d) = d

-- Wrappers
emptyPA :: PA a => PArray a
{-# INLINE_PA emptyPA #-}
emptyPA = PArray 0# emptyPD

replicatePA# :: PA a => Int# -> a -> PArray a
{-# INLINE_PA replicatePA# #-}
replicatePA# n# x = PArray n# (replicatePD n# x)

replicatelPA# :: PA a => U.Segd -> PArray a -> PArray a
{-# INLINE_PA replicatelPA# #-}
replicatelPA# segd (PArray n# xs)
  = PArray (elementsSegd# segd) (replicatelPD segd xs)

repeatPA# :: PA a => Int# -> PArray a -> PArray a
{-# INLINE_PA repeatPA# #-}
repeatPA# m# (PArray n# xs) = PArray (m# *# n#) (repeatPD m# n# xs)

indexPA# :: PA a => PArray a -> Int# -> a
{-# INLINE_PA indexPA# #-}
indexPA# (PArray _ xs) i# = indexPD xs i#

extractPA# :: PA a => PArray a -> Int# -> Int# -> PArray a
{-# INLINE_PA extractPA# #-}
extractPA# (PArray _ xs) i# n# = PArray n# (extractPD xs i# n#)

bpermutePA# :: PA a => PArray a -> Int# -> U.Array Int -> PArray a
{-# INLINE bpermutePA# #-}
bpermutePA# (PArray _ xs) n# is = PArray n# (bpermutePD xs n# is)

appPA# :: PA a => PArray a -> PArray a -> PArray a
{-# INLINE_PA appPA# #-}
appPA# (PArray m# xs) (PArray n# ys) = PArray (m# +# n#) (appPD xs ys)

applPA# :: PA a => U.Segd -> U.Segd -> PArray a -> U.Segd -> PArray a -> PArray a
{-# INLINE_PA applPA# #-}
applPA# segd is (PArray m# xs) js (PArray n# ys)
  = PArray (m# +# n#) (applPD segd is xs js ys)

packByTagPA# :: PA a => PArray a -> Int# -> U.Array Int -> Int# -> PArray a
{-# INLINE_PA packByTagPA# #-}
packByTagPA# (PArray _ xs) n# tags t# = PArray n# (packByTagPD xs n# tags t#)

combine2PA# :: PA a => Int# -> U.Sel2 -> PArray a -> PArray a -> PArray a
{-# INLINE_PA combine2PA# #-}
combine2PA# n# sel (PArray _ as) (PArray _ bs)
  = PArray n# (combine2PD n# sel as bs)

updatePA# :: PA a => PArray a -> U.Array Int -> PArray a -> PArray a
{-# INLINE_PA updatePA# #-}
updatePA# (PArray n# xs) is (PArray _ ys)
  = PArray n# (updatePD xs is ys)

fromListPA# :: PA a => Int# -> [a] -> PArray a
{-# INLINE_PA fromListPA# #-}
fromListPA# n# xs = PArray n# (fromListPD n# xs)

fromListPA :: PA a => [a] -> PArray a
{-# INLINE fromListPA #-}
fromListPA xs = case length xs of
                  I# n# -> fromListPA# n# xs

nfPA :: PA a => PArray a -> ()
{-# INLINE nfPA #-}
nfPA (PArray _ xs) = nfPD xs


-- PRScalar Operators -----------------------------------------------------------------------------
--
-- These operators are used when the array elements are known to be scalar values.
--	In this case we can use the "real" functions that operate on unboxed vector data.
--	The vector data may be processed in parallel or sequentially, depending on 
--	what vector primitive library has been linked in.
--

-- |Class of types that are known to be scalar values.
--	Holds functions to convert the PData to flat scalar vectors, and back.
--
class U.Elt a => Scalar a where
  fromScalarPData :: PData a -> U.Array a
  toScalarPData   :: U.Array a -> PData a

emptyPRScalar :: Scalar a => T_emptyPR a
{-# INLINE emptyPRScalar #-}
emptyPRScalar = toScalarPData U.empty

replicatePRScalar :: Scalar a => T_replicatePR a
{-# INLINE replicatePRScalar #-}
replicatePRScalar n# x = traceF "replicatePRScalar"
                       $ toScalarPData (U.replicate (I# n#) x)

replicatelPRScalar :: Scalar a => T_replicatelPR a
{-# INLINE replicatelPRScalar #-}
replicatelPRScalar segd xs = traceF "replicatelPRScalar"
                         $ toScalarPData
                         $ U.replicate_s segd 
                         $ fromScalarPData xs

repeatPRScalar :: Scalar a => T_repeatPR a
{-# INLINE repeatPRScalar #-}
repeatPRScalar n# len# xs = traceF "repeatPRScalar"
                        $ toScalarPData
                        $ U.repeat (I# n#) (I# len#)
                        $ fromScalarPData xs

indexPRScalar :: Scalar a => T_indexPR a
{-# INLINE indexPRScalar #-}
indexPRScalar xs i# = fromScalarPData xs U.!: I# i#

extractPRScalar :: Scalar a => T_extractPR a
{-# INLINE extractPRScalar #-}
extractPRScalar xs i# n# = traceF "extractPRScalar"
                       $ toScalarPData
                       $ U.extract (fromScalarPData xs) (I# i#) (I# n#)

bpermutePRScalar :: Scalar a => T_bpermutePR a
{-# INLINE bpermutePRScalar #-}
bpermutePRScalar xs _ is = traceF "bpermutePRScalar"
                       $ toScalarPData
                       $ U.bpermute (fromScalarPData xs) is

appPRScalar :: Scalar a => T_appPR a
{-# INLINE appPRScalar #-}
appPRScalar xs ys = traceF "appPRScalar"
                $ toScalarPData
                $ fromScalarPData xs U.+:+ fromScalarPData ys

applPRScalar :: Scalar a => T_applPR a
{-# INLINE applPRScalar #-}
applPRScalar segd xsegd xs ysegd ys
  = traceF "applPRScalar"
  $ toScalarPData
  $ U.append_s segd xsegd (fromScalarPData xs)
                    ysegd (fromScalarPData ys)
                        
packByTagPRScalar :: Scalar a => T_packByTagPR a
{-# INLINE packByTagPRScalar #-}
packByTagPRScalar xs _ tags t# = traceF "packByTagPRScalar"
                             $ toScalarPData
                             $ U.packByTag (fromScalarPData xs) tags (I# t#)

combine2PRScalar :: Scalar a => T_combine2PR a
{-# INLINE combine2PRScalar #-}
combine2PRScalar _ sel xs ys = traceF "combine2PRScalar"
                             $ toScalarPData
                             $ U.combine2 sel
                                          (fromScalarPData xs)
                                          (fromScalarPData ys)

updatePRScalar :: Scalar a => T_updatePR a
{-# INLINE updatePRScalar #-}
updatePRScalar xs is ys = traceF "updatePRScalar"
                        $ toScalarPData
                        $ U.update (fromScalarPData xs)
                                   (U.zip is (fromScalarPData ys))

fromListPRScalar :: Scalar a => T_fromListPR a
{-# INLINE fromListPRScalar #-}
fromListPRScalar _ xs = toScalarPData (U.fromList xs)

nfPRScalar :: Scalar a => T_nfPR a
{-# INLINE nfPRScalar #-}
nfPRScalar xs = fromScalarPData xs `seq` ()

