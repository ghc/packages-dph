{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}

module Types ( Point, Line, IPoint, ILine, xOf, yOf, iOf ) where

type Point = (Double,Double)
type Line  = (Point,Point)

type IPoint = (Int,Point)
type ILine  = (IPoint,IPoint)

xOf :: IPoint -> Double
xOf (_,(x,_)) = x

yOf :: IPoint -> Double
yOf (_,(_,y)) = y

iOf :: IPoint -> Int
iOf (i,_) = i

