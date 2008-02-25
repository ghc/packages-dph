        {-# GHC_OPTIONS -fglasgow-exts #-}
module BarnesHutSeq

where
import Data.Array.Parallel.Unlifted
import BarnesHutGen


type  BHTree      = [BHTreeLevel]
type  BHTreeLevel = (UArr MassPoint, UArr Int) -- centroids







eClose  = 0.5


-- Phase 1: building the tree
-- 
splitPoints:: BoundingBox -> UArr MassPoint -> SUArr MassPoint
splitPoints (ll@(llx :*: lly) :*: ru@(rux :*: ruy)) particles 
  | noOfPoints == 0 = singletonSU particles
  | otherwise          = singletonSU lls +:+^ singletonSU lus +:+^ singletonSU rus +:+^ singletonSU rls 
      where
        noOfPoints = lengthU particles
        lls           = filterU (inBox (ll :*: mid)) particles 
        lus           = filterU (inBox ((llx :*: midy)  :*: (midx :*: ruy ))) particles 
        rus           = filterU (inBox (mid             :*: ru             )) particles 
        rls           = filterU (inBox ((midx :*: lly)  :*: (rux  :*: midy))) particles 
   
        mid@(midx :*: midy) = ((llx + rux)/2.0) :*: ((lly + ruy)/2.0) 



splitPointsL::  UArr BoundingBox -> SUArr MassPoint -> BHTree
splitPointsL  bboxes particless
  | lengthSU multiparticles == 0 =  [(centroids, toU [])]
  | otherwise              = (centroids, lengthsSU multiparticles) : 
     (splitPointsL newBoxes multiparticles)
  where
                           
    multiparticles = (splitPointsL' llbb lubb rubb rlbb) $ 
       packCU multiPointFlags particless

    centroids =  
      calcCentroids $ segmentArrU nonEmptySegd $ flattenSU particless
    
    multiPointFlags = mapU ((>1)) $ lengthsSU particless 
    nonEmptySegd = filterU ((>0)) $ lengthsSU particless 
    bboxes' = packU bboxes multiPointFlags
    
    newBoxes = merge4 llbb lubb rubb rlbb 

    llbb = mapU makells bboxes'
    lubb = mapU makelus bboxes'
    rubb = mapU makerus bboxes'
    rlbb = mapU makerls bboxes'

    makells (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            ll :*: (((llx + rux)/2.0) :*: (((lly + ruy)/2.0)))
    makelus (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            (llx :*: ((lly + ruy)/2.0))  :*: (((llx + rux)/2.0) :*: ruy )
    makerus (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            (((llx + rux)/2.0) :*: ((lly + ruy)/2.0)) :*: ru    
    makerls (ll@(llx :*: lly) :*: ru@(rux :*: ruy))  = 
            ((((llx + rux)/2.0) :*: lly)  :*: (rux  :*: ((lly + ruy)/2.0)))
        


calcCentroids:: SUArr MassPoint -> UArr MassPoint
calcCentroids orderedPoints = centroids
  where
    ms = foldSU (+) 0.0 $ sndSU orderedPoints
    centroids = zipWithU div' ms $
           foldSU pairP (0.0 :*: 0.0) $
            zipWithSU multCoor orderedPoints 
              (replicateSU (lengthsSU orderedPoints) ms)
    div' m (x :*: y) = ((x/m :*: y/m)   :*: m)
    multCoor ((x :*: y)  :*: _)  m = (m * x :*: y *x)

    pairP (x1 :*: y1) (x2 :*: y2) = ((x1+x2) :*: (y1 + y2))


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
          flattenSU $ appendSU llsPs (appendSU lusPs (appendSU rusPs rlsPs))
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


-- phase 2:
--   calculating the velocities

calcVelocity:: BHTree -> UArr MassPoint ->  UArr (Double :*: Double)
calcVelocity [] particles 
  | lengthU particles == 0 = emptyU
  | otherwise              = error $ "calcVelocity: reached empty tree" ++ (show particles)
calcVelocity  ((centroids, segd) :trees) particles = closeVelo
  where

    closeVelo = splitApplyU  particlesClose
                  ((calcVelocity trees) . sndU )
                  calcFarVelocity 
                  (zipU
                       (flattenSU $ replicateCU (lengthU particles) centroids)
                       (flattenSU $ replicateSU segd particles))
    particlesClose (((x1 :*: y1):*: _)  :*: ((x2 :*: y2) :*: _))  =  
        (x1-x2)^2 + (y1-y2)^2 < eClose
    
calcFarVelocity      = mapU accel


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
    flags1 = mapU even $ enumFromToU 0 (2 * len-1)
    flags2 = mapU (\x -> mod x 3 /= 2) $ enumFromToU 0 (3 * len-1)
    flags3 = mapU (\x -> mod x 4 /= 3) $ enumFromToU 0 (4 * len-1)
    len    = lengthU a1

-- checks if particle is in box (excluding left and lower border)
inBox:: BoundingBox -> MassPoint -> Bool
inBox ((ll@(llx :*: lly) :*: ru@(rux :*: ruy))) ((px :*: py) :*: _) =
  (px > llx) && (px <= rux) && (py > lly) && (py <= ruy)




--  General functions which should be in the library

--

splitApplyU:: (UA e, UA e') =>  (e -> Bool) -> (UArr e -> UArr e') -> (UArr e -> UArr e') -> UArr e -> UArr e'
splitApplyU p f1 f2 xsArr = combineU (mapU p xsArr) res1 res2
  where
    res1 = f1 $ filterU p xsArr
    res2 = f2 $ filterU (not . p) xsArr

splitApplySU:: (UA e, UA e') =>  UArr Bool -> (SUArr e -> SUArr e') -> (SUArr e -> SUArr e') -> SUArr e -> SUArr e'
{-# INLINE splitApplySU #-}
splitApplySU  flags f1 f2 xssArr = combineCU flags res1 res2
  where
    res1 = f1 $ packCU flags xssArr 
    res2 = f2 $ packCU (mapU not flags) xssArr

packCU:: (UA e) => UArr Bool -> SUArr e -> SUArr e
{-# INLINE packCU #-}
packCU flags xssArr = segmentArrU newLengths flatData
  where
    repFlags   = flattenSU $ replicateSU (lengthsSU xssArr) flags
    flatData   = packU (flattenSU xssArr) repFlags  
    newLengths = packU (lengthsSU xssArr) flags    


appendSU:: (UA e) => SUArr e -> SUArr e -> SUArr e
{-# INLINE appendSU #-}
appendSU xssArr1 xssArr2 = segmentArrU newLengths flatData 
  where
    len        = lengthSU xssArr1 + lengthSU xssArr2
    flags      = mapU even $ enumFromToU 0 (len-1)
    flatData   = flattenSU $ combineCU flags xssArr1 xssArr2 
    newLengths = zipWithU (+) (lengthsSU xssArr1) (lengthsSU xssArr2)


