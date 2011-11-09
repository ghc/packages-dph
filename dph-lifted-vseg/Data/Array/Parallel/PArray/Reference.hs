
-- | During testing, we compare the output of each invocation of the lifted
--   combinators in D.A.P.PArray with the reference implementations. 
--
--   This module helps convert the to and from the array representation
--   used by the reference implementation.
--
--   TODO: we could use this to trace the lengths of the vectors being used, 
--         as well as the types that each opeartor is being called at.
--
module Data.Array.Parallel.PArray.Reference
        ( withRef1, withRef2
        , toRef1,   toRef2,   toRef3)
where
import Data.Array.Parallel.PArray.PData
import Data.Array.Parallel.PArray.PRepr
import Debug.Trace
import qualified Data.Array.Parallel.Array      as A
import qualified Data.Array.Parallel.Pretty     as T
import qualified Data.Vector                    as V
import qualified "dph-lifted-reference" 
                 Data.Array.Parallel.PArray     as R
import Prelude hiding (length)

-- Config ---------------------------------------------------------------------
debugLiftedTrace        :: Bool
debugLiftedTrace        = False

debugLiftedCompare      :: Bool
debugLiftedCompare      = False


-- withRef --------------------------------------------------------------------
-- | Compare the result of some array operator against a reference.
withRef1 :: (PA a, PA (c a), A.Array c a)
         => String                 -- name of operator
         -> R.PArray a             -- result using reference implementation
         -> c a                    -- result using vseg implementation
         -> c a

withRef1 name arrRef arrImpl
 = let  trace'
         = if debugLiftedTrace  
            then trace (T.render $ T.text " " 
                        T.$$ T.text name 
                        T.$$ (T.nest 8 $ pprpPA arrImpl))
            else id    

        resultOk
         = A.valid arrImpl
             && A.length arrRef == A.length arrImpl
             && (V.and $ V.zipWith
                  similarPA
                  (A.toVectors1 arrRef) (A.toVectors1 arrImpl))
              
        resultFail
         = error $ T.render $ T.vcat
                [ T.text "withRef1: failure " T.<> T.text name
                , T.nest 4 $ pprp  $ A.toVectors1 arrRef
                , T.nest 4 $ pprpPA arrImpl ]

   in   trace' (if debugLiftedCompare
                 then (if resultOk then arrImpl else resultFail)
                 else arrImpl)
{-# INLINE withRef1 #-}


-- | Compare the nested result of some array operator against a reference.
withRef2 :: ( A.Array c (c a), PA (c (c a))
            , A.Array c a,     PA (c a)
            , PA a)
         => String                 -- name of operator.
         -> R.PArray (R.PArray a)  -- result using reference implementaiton.
         -> c (c a)                -- result using vseg implementation.
         -> c (c a)

withRef2 name arrRef arrImpl
 = let  trace'
         = if debugLiftedTrace  
            then trace (T.render $ T.text " " 
                        T.$$ T.text name 
                        T.$$ (T.nest 8 $ pprpPA arrImpl))
            else id

        resultOK
         = A.valid arrImpl
           && A.length arrRef == A.length arrImpl
           && (V.and $ V.zipWith 
                (\xs ys -> V.and $ V.zipWith similarPA xs ys)
                (A.toVectors2 arrRef) (A.toVectors2 arrImpl))
        
        resultFail
         = error $ T.render $ T.vcat
                [ T.text "withRef2: failure " T.<> T.text name
                , T.nest 4 $ pprpPA arrImpl ]

   in   trace' (if debugLiftedCompare
                 then (if resultOK then arrImpl else resultFail)
                 else arrImpl)
{-# INLINE withRef2 #-}


-- toRef ----------------------------------------------------------------------
-- | Convert an array to the reference version.
toRef1  :: (A.Array c a, PA (c a))
        => c a -> R.PArray a

toRef1  = A.fromVectors1 . A.toVectors1

-- | Convert a nested array to the reference version.
toRef2 :: ( A.Array c (c a), PA (c a)
          , A.Array c a) 
       => c (c a)
       -> R.PArray (R.PArray a)

toRef2  = A.fromVectors2 . A.toVectors2

-- | Convert a doubly nested array to the reference version.
toRef3 :: ( A.Array c (c (c a)), PA (c (c a))
          , A.Array c (c a),     PA (c a)
          , A.Array c a)
       => c (c (c a))
       -> R.PArray (R.PArray (R.PArray a))

toRef3  = A.fromVectors3 . A.toVectors3

