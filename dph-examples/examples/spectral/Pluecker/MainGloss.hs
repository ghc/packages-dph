{-# LANGUAGE BangPatterns #-}

import Common
import Solver

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate

import System.Environment
import Data.Maybe
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU

import System.Random

model :: Int -> Int -> IO (VU.Vector Vec3, VU.Vector (Int,Int,Int))
model numverts numtris
 = do
   vs <- VU.generateM numverts mkvec
   ts <- VU.generateM numtris  mktri
   return (vs,ts)
 where
   mkvec _ = do
     x <- randomRIO (-10,10)
     y <- randomRIO (-10,10)
     z <- randomRIO (0,10)
     return (x,y,z)
   mktri _ = do
     a <- randomRIO (0,numverts - 1)
     b <- randomRIO (0,numverts - 1)
     c <- randomRIO (0,numverts - 1)
     return (a,b,c)


main :: IO ()
main  
 = do   args    <- getArgs
        mainWithArgs args
        

mainWithArgs :: [String] -> IO ()
mainWithArgs [solverName,numv,numt,numr]
 = let  -- The solver we're using to calculate the acclerations.
        solver      = fromMaybe (error $ unlines
                                        [ "unknown solver " ++ show solverName
                                        , "choose one of "  ++ (show $ map fst solvers) ])
                        $ lookup solverName solvers
    in do
        (v,t)   <- model (read numv) (read numt)
        --let v = VU.fromList [(0,0,0),(1,1,1),(1,0,1)]
        --let t = VU.fromList [(0,1,2)]
        mainGloss v t (read numr) solver 400

mainWithArgs [solverName] = mainWithArgs [solverName,"100","100","1000"]

mainWithArgs _ = putStrLn "Usage: pluecker <vector|vectorised> <verts=100> <tris=100> <rays=1000>"


-- | Run the simulation in a gloss window.
mainGloss 
        :: VU.Vector Vec3
        -> VU.Vector (Int,Int,Int)
        -> Int          -- ^ number of rays
        -> Solver       -- ^ Fn to calculate accels of each point.
        -> Int          -- ^ Size of window.
        -> IO ()
        
mainGloss v t numr solver windowSize
 = let  mkray _
         = do   x <- randomRIO (-3,3)
                y <- randomRIO (-3,3)
                return (x,y,1)
            
        draw time
         = do   rays <- VU.generateM numr mkray
                let pts = solver v t rays (realToFrac time)
                return $ Pictures $ map drawPoint $ VU.toList pts

   in   animateIO
                (InWindow  "Silly"                    -- window name
                           (windowSize, windowSize)   -- window size
                           (10, 10))                  -- window position
                black                                 -- background color
                draw                                  -- fn to convert a world to a picture



pointSize = 15

drawPoint :: (Vec3, Double) -> Picture
drawPoint ((x,y,z), d)
	= Translate (realToFrac x * 200) (realToFrac y * 200) 
    $ Color (makeColor d' d' d' 0.5)
    $ Polygon [ (-pointSize, -pointSize)
              , ( pointSize, -pointSize)
              , ( pointSize,  pointSize)
              , (-pointSize,  pointSize) ]
	-- $ ThickCircle (pointSize / 2) pointSize
 where d' = 1 - (realToFrac $ d / 20)
