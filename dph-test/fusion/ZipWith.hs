

module Test where
import Data.Vector.Unboxed              as V
import Prelude                          as P
import Unboxed


test    :: Vector Int
        -> Vector Int
        -> Vector Int
        -> Vector (Int, Int, Int)

test aa bb cc
        = V.zipWith3 (,,) aa bb cc



test2   :: Vector Int
        -> Vector Int
        -> Vector Int
        -> Vector (Int, Int, Int)

test2 aa bb cc
        = lockedZip3 aa bb cc

