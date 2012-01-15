{-# OPTIONS_GHC -fvectorise #-}

module Data.Array.Parallel.Prelude.Word8
        ( Word8
          
        -- * Ord
        , (==), (/=), (<), (<=), (>), (>=), min, max
        , maximumP,  minimumP
        , maxIndexP, minIndexP
     
        -- * Num
        , (+), (-), (*)
        , negate, abs
        , sumP, productP
        
        -- * Integral
        , div, mod, sqrt
        
        -- * Conversion
        , fromInt
        , toInt)
where
import Data.Array.Parallel.Prim                         ()      
import Data.Array.Parallel.Prelude.Base                 (Bool, Int, Word8, Eq, Ord, Num)
import Data.Array.Parallel.PArr
import Data.Array.Parallel.PArray
import Data.Array.Parallel.Lifted                       ((:->)(..))
import qualified Data.Array.Parallel.Lifted             as L
import qualified Data.Array.Parallel.PArray.Scalar      as SC
import qualified Prelude as P


{-# VECTORISE SCALAR instance Eq  Word8 #-}
{-# VECTORISE SCALAR instance Ord Word8 #-}
{-# VECTORISE SCALAR instance Num Word8 #-}


infixl 7 *
infixl 6 +, -
infix  4 ==, /=, <, <=, >, >=
infixl 7 `div`, `mod`

-- Ord ------------------------------------------------------------------------
(==), (/=), (<), (<=), (>), (>=) :: Word8 -> Word8 -> Bool

(==) = (P.==)
{-# VECTORISE SCALAR (==) #-}

(/=) = (P./=)
{-# VECTORISE SCALAR (/=) #-}

(<=) = (P.<=)
{-# VECTORISE SCALAR (<=) #-}

(<)  = (P.<)
{-# VECTORISE SCALAR (<) #-}

(>=) = (P.>=)
{-# VECTORISE SCALAR (>=) #-}

(>)  = (P.>)
{-# VECTORISE SCALAR (>) #-}


-- min/max ----------------------------
min, max :: Word8 -> Word8 -> Word8

min = P.min
{-# VECTORISE SCALAR min #-}

max = P.max
{-# VECTORISE SCALAR max #-}


-- minimum/maximum --------------------
minimumP, maximumP :: PArr Word8 -> Word8

minimumP arr    = headPArr arr
{-# NOINLINE  minimumP #-}
{-# VECTORISE minimumP = minimumPP #-}

maximumP arr    = headPArr arr
{-# NOINLINE  maximumP #-}
{-# VECTORISE maximumP = maximumPP #-}

minimumPP, maximumPP :: PArray Word8 :-> Word8
minimumPP      = L.closure1' (SC.fold1 P.min) (SC.fold1s P.min)
{-# INLINE      minimumPP #-}
{-# NOVECTORISE minimumPP #-}

maximumPP      = L.closure1' (SC.fold1 P.max) (SC.fold1s P.max)
{-# INLINE      maximumPP #-}
{-# NOVECTORISE maximumPP #-}


-- minIndex/maxIndex ------------------
minIndexP :: PArr Word8 -> Int
minIndexP !_    = 0 
{-# NOINLINE  minIndexP #-}
{-# VECTORISE minIndexP = minIndexPP #-}

minIndexPP :: PArray Word8 :-> Int
minIndexPP      = L.closure1' (SC.fold1Index min') (SC.fold1sIndex min')
{-# INLINE      minIndexPP #-}
{-# NOVECTORISE minIndexPP #-}

min' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
min' (i,x) (j,y) | x P.<= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE min' #-}


maxIndexP :: PArr Word8 -> Int
maxIndexP _     = 0
{-# NOINLINE  maxIndexP #-}
{-# VECTORISE maxIndexP = maxIndexPP #-}

maxIndexPP :: PArray Word8 :-> Int
maxIndexPP      = L.closure1' (SC.fold1Index max') (SC.fold1sIndex max')
{-# INLINE      maxIndexPP #-}
{-# NOVECTORISE maxIndexPP #-}

max' :: P.Ord b => (a, b) -> (a, b) -> (a, b)
max' (i,x) (j,y) | x P.>= y    = (i,x)
                 | P.otherwise = (j,y)
{-# NOVECTORISE max' #-}


-- Num ------------------------------------------------------------------------
(+), (-), (*) :: Word8 -> Word8 -> Word8

(+) = (P.+)
{-# VECTORISE SCALAR (+) #-}

(-) = (P.-)
{-# VECTORISE SCALAR (-) #-}

(*) = (P.*)
{-# VECTORISE SCALAR (*) #-}


-- negate/abs -------------------------
negate, abs :: Word8 -> Word8

negate  = P.negate
{-# VECTORISE SCALAR negate #-}

abs     = P.abs
{-# VECTORISE SCALAR abs #-}


-- sum/product ------------------------
sumP, productP :: PArr Word8 -> Word8

sumP arr        = headPArr arr
{-# NOINLINE  sumP #-}
{-# VECTORISE sumP      = sumPP #-}

productP arr    = headPArr arr
{-# NOINLINE  productP #-}
{-# VECTORISE productP  = productPP #-}

sumPP, productPP :: PArray Word8 :-> Word8
sumPP          = L.closure1' (SC.fold (+) 0) (SC.folds (+) 0)
{-# INLINE      sumPP #-}
{-# NOVECTORISE sumPP #-}

productPP      = L.closure1' (SC.fold (*) 1) (SC.folds (*) 1)
{-# INLINE      productPP #-}
{-# NOVECTORISE productPP #-}


-- Integral -------------------------------------------------------------------
div, mod :: Word8 -> Word8 -> Word8

div = P.div
{-# VECTORISE SCALAR div #-}

mod = P.mod
{-# VECTORISE SCALAR mod #-}


sqrt :: Word8 -> Word8 
sqrt n = P.floor (P.sqrt (P.fromIntegral n) :: P.Double)
{-# VECTORISE SCALAR sqrt #-}


-- Conversion -----------------------------------------------------------------
toInt :: Word8 -> Int
toInt = P.fromIntegral
{-# VECTORISE SCALAR toInt #-}

fromInt :: Int -> Word8
fromInt = P.fromIntegral
{-# VECTORISE SCALAR fromInt #-}
