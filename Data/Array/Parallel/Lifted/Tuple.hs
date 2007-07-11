module Data.Array.Parallel.Lifted.Tuple
where

import Data.Array.Parallel.Lifted.PArray

data instance PArray () = PUnit !Int ()

instance PA () where
  {-# INLINE lengthPA #-}
  lengthPA    (PUnit n _) = n
  {-# INLINE replicatePA #-}
  replicatePA n x         = PUnit n x

-- Tuples
--
data STup2 a b = STup2 !a !b
data STup3 a b c = STup3 !a !b !c
data STup4 a b c d = STup4 !a !b !c !d
data STup5 a b c d e = STup5 !a !b !c !d !e

data instance PArray (a,b) = PTup2 !Int (STup2 (PArray a)
                                               (PArray b))

instance (PA a, PA b) => PA (a,b) where
  {-# INLINE lengthPA #-}
  lengthPA (PTup2 n _) = n
  {-# INLINE replicatePA #-}
  replicatePA n p = PTup2 n
                    (case p of { (a,b) ->
                       STup2 (replicatePA n a)
                             (replicatePA n b)})

data instance PArray (a,b,c) = PTup3 !Int (STup3 (PArray a)
                                                 (PArray b)
                                                 (PArray c))

instance (PA a, PA b, PA c) => PA (a,b,c) where
  {-# INLINE lengthPA #-}
  lengthPA (PTup3 n _) = n
  {-# INLINE replicatePA #-}
  replicatePA n p = PTup3 n
                    (case p of { (a,b,c) ->
                       STup3 (replicatePA n a)
                             (replicatePA n b)
                             (replicatePA n c)})

