{-# LANGUAGE TypeOperators #-}
module BarnesHutPar (splitPointsLPar)

where

import Data.Array.Parallel.Unlifted.Distributed
import Data.Array.Parallel.Unlifted.Parallel
import Data.Array.Parallel.Unlifted.Sequential
import Data.Array.Parallel.Base ( (:*:)(..), sndS, uncurryS )

import BarnesHutGen






-- Phase 1: building the tree
--

-- Split massPoints according to their locations in the quadrants
-- 
splitPoints:: BoundingBox -> UArr MassPoint -> SUArr MassPoint
splitPoints (ll@(llx :*: lly) :*: ru@(rux :*: ruy)) particles 
  | noOfPoints == 0 = singletonSU particles
  | otherwise          = singletonSU lls +:+^ singletonSU lus +:+^ singletonSU rus +:+^ singletonSU rls 
      where
        noOfPoints = lengthU particles
        lls           = filterUP (inBox (ll :*: mid)) particles 
        lus           = filterUP (inBox ((llx :*: midy)  :*: (midx :*: ruy ))) particles 
        rus           = filterUP (inBox (mid             :*: ru             )) particles 
        rls           = filterUP (inBox ((midx :*: lly)  :*: (rux  :*: midy))) particles 
   
        mid@(midx :*: midy) = ((llx + rux)/2.0) :*: ((lly + ruy)/2.0) 



splitPointsLPar::  UArr BoundingBox -> SUArr MassPoint -> BHTree
splitPointsLPar  bboxes particless
  | lengthSU multiparticles == 0 =  [(centroids, toU [])]
  | otherwise              = (centroids, lengthsSU multiparticles) : 
     (splitPointsLPar newBoxes multiparticles)
  where
    -- calculate centroid of each segment
    centroids =  
      calcCentroids $ segmentArrU nonEmptySegd $ flattenSU particless
                           
    -- remove empty segments
    multiPointFlags = mapUP ((>1)) $ lengthsSU particless 
    multiparticles = (splitPointsL' llbb lubb rubb rlbb) $ 
       packCUP multiPointFlags particless
    bboxes' = packUP bboxes multiPointFlags
    
    nonEmptySegd = filterUP ((>0)) $ lengthsSU particless 

    -- split each box in four sub-boxes    
    newBoxes = merge4 llbb lubb rubb rlbb 

    llbb = mapUP makells bboxes'
    lubb = mapUP makelus bboxes'
    rubb = mapUP makerus bboxes'
    rlbb = mapUP makerls bboxes'

    makells (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            ll :*: (((llx + rux)/2.0) :*: (((lly + ruy)/2.0)))
    makelus (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            (llx :*: ((lly + ruy)/2.0))  :*: (((llx + rux)/2.0) :*: ruy )
    makerus (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            (((llx + rux)/2.0) :*: ((lly + ruy)/2.0)) :*: ru    
    makerls (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            ((((llx + rux)/2.0) :*: lly)  :*: (rux  :*: ((lly + ruy)/2.0)))
        
splitPointsL':: UArr BoundingBox -> 
  UArr BoundingBox -> 
  UArr BoundingBox -> 
  UArr BoundingBox -> 
  SUArr MassPoint -> 
  SUArr MassPoint
splitPointsL' llbb lubb rubb rlbb  particless
  | particlessLen == 0 = particless
  | otherwise          = orderedPoints
      where

        -- each segment split into four subsegments with particles located in 
        -- the four quadrants
        orderedPoints = 
          segmentArrU newLengths $
          flattenSU $ llsPs  ^+:+^ lusPs ^+:+^ rusPs ^+:+^ rlsPs
        particlessLen = lengthSU particless
        pssLens = lengthsSU particless
        lls = replicateSU pssLens llbb
        lus = replicateSU pssLens lubb
        rus = replicateSU pssLens rubb
        rls = replicateSU pssLens rlbb


        llsPs = mapSU sndS $ filterSU (uncurryS inBox)  
          (zipSU (replicateSU pssLens llbb) particless)
        lusPs = mapSU sndS $ filterSU (uncurryS inBox)  
          (zipSU (replicateSU pssLens lubb) particless)
        rusPs = mapSU sndS $ filterSU (uncurryS inBox)  
          (zipSU (replicateSU pssLens rubb) particless)
        rlsPs = mapSU sndS $ filterSU (uncurryS inBox)  
          (zipSU (replicateSU pssLens rlbb) particless)

        newLengths = 
          merge4 (lengthsSU llsPs) (lengthsSU lusPs) 
                 (lengthsSU rusPs) (lengthsSU rlsPs)


-- Calculate centroid of each subarray
--
calcCentroids:: SUArr MassPoint -> UArr MassPoint
calcCentroids orderedPoints = centroids
  where
    ms = foldSU (+) 0.0 $ sndSU orderedPoints
    centroids = zipWithUP div' ms $
           foldSU pairP (0.0 :*: 0.0) $
            zipWithSU multCoor orderedPoints 
              (replicateSU (lengthsSU orderedPoints) ms)
    div' m (x :*: y) = ((x/m :*: y/m)   :*: m)
    multCoor ((x :*: y)  :*: _)  m = (m * x :*: m * y)

    pairP (x1 :*: y1) (x2 :*: y2) = ((x1+x2) :*: (y1 + y2))



-- phase 2:
--   calculating the velocities

calcAccel:: BHTree -> UArr MassPoint ->  UArr (Double :*: Double)
calcAccel [] particles 
  | lengthU particles == 0 = emptyU
  | otherwise              = error $ "calcVelocity: reached empty tree" ++ (show particles)
calcAccel  ((centroids, segd) :trees) particles = closeAccel
  where

    closeAccel = splitApplyU  particlesClose
                    ((calcAccel trees) . sndU )
                    calcFarAccel 
                    (zipU
                       (flattenSU $ replicateCU (lengthU particles) centroids)
                       (flattenSU $ replicateSU segd particles))
    particlesClose (((x1 :*: y1):*: _)  :*: ((x2 :*: y2) :*: _))  =  
        (x1-x2)^2 + (y1-y2)^2 < eClose
    
calcFarAccel:: UArr (MassPoint :*: MassPoint) -> UArr Accel
calcFarAccel      = mapUP accel

-- 
-- 
accel:: MassPoint :*: MassPoint -> Accel
accel (((x1:*: y1) :*: m)  :*:
      ((x2:*: y2) :*: _)) | r < epsilon  = (0.0 :*: 0.0) 
                          | otherwise    = (aabs * dx / r :*: aabs * dy / r)  
                                             where 
                                               rsqr = (dx * dx) + (dy * dy) 
                                               r    = sqrt rsqr 
                                               dx   = x1 - x2 
                                               dy   = y1 - y2 
                                               aabs = m / rsqr 





-- assumes all arr have the same length
-- result [a11, a21, a31, a41, a12, a22....]
merge4:: UA a => 
  UArr a ->UArr a ->UArr a ->UArr a ->UArr a
merge4 a1 a2 a3 a4 = 
  combineU flags3 (combineU flags2 (combineU flags1 a1 a2) a3) a4
  where
    flags1 = mapUP even $ enumFromToU 0 (2 * len-1)
    flags2 = mapUP (\x -> mod x 3 /= 2) $ enumFromToU 0 (3 * len-1)
    flags3 = mapUP (\x -> mod x 4 /= 3) $ enumFromToU 0 (4 * len-1)
    len    = lengthU a1

-- checks if particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
inBox ((ll@(llx :*: lly) :*: ru@(rux :*: ruy))) ((px :*: py) :*: _) =
  (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)





splitApplyU:: (UA e, UA e') =>  (e -> Bool) -> (UArr e -> UArr e') -> (UArr e -> UArr e') -> UArr e -> UArr e'
splitApplyU p f1 f2 xsArr = combineU (mapUP p xsArr) res1 res2
  where
    res1 = f1 $ filterUP p xsArr
    res2 = f2 $ filterUP (not . p) xsArr

splitApplySU:: (UA e, UA e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = combineCU flags res1 res2
  where
    res1 = f1 $ packCUP flags xssArr 
    res2 = f2 $ packCUP (mapUP not flags) xssArr





