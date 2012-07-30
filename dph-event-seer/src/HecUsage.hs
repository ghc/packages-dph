-- HEC usage analysis.
--      Count time spent with none, some and all HECs active or in garbage collection.
module HecUsage where

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

-- | Count results as either N HECs active, or some GC going on
data HecCurrentCap = HecCap Int | HecGC
    deriving (Show,Eq,Ord)

data HecUsageState = HecUsageState
    (Map HecCurrentCap Timestamp) -- ^ results map of active HECs to total time spent
    Int                           -- ^ threads running
    Int                           -- ^ threads in GC
    Timestamp                     -- ^ time of previous event
    Timestamp                     -- ^ total time spent

hecUsageMachine :: Machine HecUsageState CapEvent
hecUsageMachine = Machine
    { initial = HecUsageState M.empty 0 0 0 0
    , final   = const False
    , alpha   = alph
    , delta   = delt
    }
 where
  -- Ignore events with no HEC associated
  alph (CapEvent Nothing _) = False
  alph (CapEvent _ (Event _ evt)) = alph_evt evt
  alph _ = False

  -- Only interested in threads starting, stopping, and GC events
  alph_evt (RunThread _)        = True
  alph_evt (StopThread _ _)     = True
  alph_evt (StartGC)            = True
  alph_evt (EndGC)              = True
  alph_evt _                    = False

  {-# INLINE delt #-}
  delt (HecUsageState counts runR runG timelast timetotal)
        (CapEvent (Just _cap) (Event timenow evtinfo))
   = let
        -- Update number of active threads and GCs
        (runR',runG')
          = case evtinfo of
              RunThread _     -> (runR + 1, runG)
              StopThread _ _  -> (runR - 1, runG)
              StartGC         -> (runR, runG + 1)
              EndGC           -> (runR, runG - 1)
              _               -> (runR, runG)
        diff    = timenow - timelast
        -- Insert into results
        counts' = update counts (hecCapOf runR runG) diff
        -- Count total time
        total'  = timetotal + diff
     in
        Just $ HecUsageState counts' runR' runG' timenow total'
  delt _ _ = Nothing

  -- If HECs are active and GC is running, count it as active time.
  -- Only count GC time if no HECs active.
  hecCapOf 0 0          = HecCap 0
  hecCapOf 0 n | n > 0  = HecGC
  hecCapOf n _          = HecCap n

  update !counts !k !v = M.insertWith' (+) k v counts


showValidate :: (s -> String) -> (i -> String) -> Either (s, i) s -> String
showValidate showState showInput (Left (state, input)) =
  "Invalid eventlog:"
  ++ "\nState:\n" ++ ( showState state )
  ++ "\nInput:\n" ++ ( showInput input )
showValidate showState _ (Right state) =
  "Valid eventlog: " ++ ( showState state )

showMap :: Ord k => (k -> String) -> (a -> String) -> M.Map k a -> String
showMap showKey showValue m =
  concat $ zipWith (++)
    (map showKey . M.keys $ m :: [String])
    (map (showValue . (M.!) m) . M.keys $ m :: [String])

showHecUsage :: HecUsageState -> String
showHecUsage (HecUsageState counts runR runG _ total) = "\n" ++ counts' ++ running'
 where
  counts' = showMap show (\v -> ":\t" ++ show (v * 100 `div` total) ++ "%\t(" ++ show v ++ ")\n") counts
  running'= if (runR,runG) == (0,0) then "" else "\nSome threads still running? " ++ show (runR,runG)
