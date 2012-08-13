module DphOps
        ( dphOpsMachine
        , dphOpsSumMachine
        , pprDphOpsState
        , pprDphOpsSumState
        , getGangEvents
        , pprGangEvents)
where

import qualified Data.Array.Parallel.Unlifted.Distributed.What as W

import GHC.RTS.Events
import GHC.RTS.Events.Analysis

import Data.Map (Map)
import qualified Data.Map as M

import Data.List        (sortBy, stripPrefix)
import Data.Function    (on)

import Pretty

import Debug.Trace

-- | result of attempting to parse a CapEvent
data ParseComp
    = POk W.Comp Timestamp -- ^ valid Comp and its duration
    | PUnparsable        -- ^ looks like a comp, but invalid
    | PIgnored           -- ^ not a comp at all, just ignore
-- | attempt to get a Comp
-- eg "GANG Complete par CompMap{...} in 44us"
parseComp :: GangEvent -> ParseComp
parseComp (GangEvent _ _ msg)
   | Just compStr   <- stripPrefix "Complete par " msg
   , (comp,inStr):_ <- reads compStr
   , Just muStr     <- stripPrefix " in " inStr
   , (mus,"us."):_  <- reads muStr
   = POk comp (mus*10^3)
   -- prefix is there, but the rest didn't parse properly
   | Just compStr   <- stripPrefix "Complete par " msg
   = PUnparsable
   -- prefix isn't there, so just ignore it
   | otherwise
   = PIgnored
parseComp _
   = PIgnored

data GangEvent = GangEvent (Maybe Int) Timestamp String
        deriving Show

-- | potentially merge multiple CapEvents into single GangEvent
getGangEvents :: [CapEvent] -> [GangEvent]
getGangEvents xs = snd $ foldl (flip gang) (Nothing,[]) xs
 where
  -- GANG[1/4] Complete par ...
  gang (CapEvent c (Event t (UserMessage msg)))
       (no,ls)
   | Just numS      <- stripPrefix "GANG[" msg
   , [(n,'/':n2S)]  <- reads numS :: [(Int,String)]
   , [(m,']':' ':restS)]<- reads n2S  :: [(Int,String)]
   = app (n,m) no (GangEvent c t restS) ls
   | otherwise
   = (no,ls)
  gang _ acc                = acc

  app (n,m) se ge ls
        | n == 1 && m == 1
        = (Nothing, ls ++ [ge])
        | n == 1
        = (Just ge, ls)
        | n == m
        = (Nothing, ls ++ [se `merge` ge])
        | otherwise
        = (Just $ se `merge` ge, ls)
        

  merge Nothing ge
        = ge
  merge (Just (GangEvent c t s)) (GangEvent _ _ s')
        = GangEvent c t (s++s')

pprGangEvents :: [GangEvent] -> Doc
pprGangEvents gs = vcat $ map ppr gs
instance Pretty GangEvent where
  ppr (GangEvent c t s) =
        padL 10 (text $ show c) <>
        padR 15 (pprTimestampAbs t) <>
        text s

-- Display all operations and duration, ordered decreasing
data DphOpsState = DphOpsState (Map Timestamp [W.Comp]) Timestamp Int

dphOpsMachine :: Machine DphOpsState GangEvent
dphOpsMachine = Machine
  { initial = DphOpsState M.empty 0 0
  , final   = const False
  , alpha   = alph
  , delta   = delt
  }
 where
  alph _ = True

  delt (DphOpsState ops total unparse) evt
   = case parseComp evt of
     POk comp duration  ->
      Just $ DphOpsState (update ops duration comp) (total + duration) unparse
     PUnparsable        ->
      Just $ DphOpsState ops total (unparse+1)
     PIgnored           ->
      Just $ DphOpsState ops total unparse
  delt s _ = Just s

  update !counts !k !v = M.insertWith' (++) k [v] counts


pprDphOpsState :: DphOpsState -> Doc
pprDphOpsState (DphOpsState ops total unparse) = vcat [unparse', ops']
 where
  unparse'
        = if unparse == 0
          then text ""
          else text "Errors: " <> ppr unparse <> text " events unable to be parsed"

  ops' = vcat $ map pprOps $ reverse $ M.assocs ops

  pprOps (duration,cs) = vcat $ map (pprOp duration) cs
  pprOp duration c     = padLines (cat
        [ pprPercent duration
        , text " "
        , padR 10 (text "(" <> pprTimestampEng duration <> text ")")
        , text " "
        ]) (show $ ppr c)
  pprPercent   v = padR 5  $ ppr (v * 100 `div` total) <> text "%"

padLines left right
 = let (x:xs) = chunks' trunc_len right
       pad'   = text $ replicate (length (render left)) ' '
   in  vcat ((left <> text x) : map (\x-> pad' <> text x) xs)

trunc_len = 100
trunc l
  | length l > trunc_len
  = take (trunc_len-4) l ++ " ..."
  | otherwise
  = l

data DphOpsSumState = DphOpsSumState (Map W.Comp (Int,Timestamp)) Timestamp Int

dphOpsSumMachine :: Machine DphOpsSumState GangEvent
dphOpsSumMachine = Machine
  { initial = DphOpsSumState M.empty 0 0
  , final   = const False
  , alpha   = alph
  , delta   = delt
  }
 where
  alph _ = True

  -- "GANG Complete par CompMap{...} in 44us"
  delt (DphOpsSumState ops total unparse) evt
   = case parseComp evt of
     POk comp duration  ->
      Just $ DphOpsSumState (update ops (clearJoinComp comp) (1,duration)) (total + duration) unparse
     PUnparsable        ->
      Just $ DphOpsSumState ops total (unparse+1)
     PIgnored           ->
      Just $ DphOpsSumState ops total unparse
  delt s _ = Just s

  update !counts !k !v = M.insertWith' pairAdd k v counts
  pairAdd (aa,ab) (ba,bb) = (aa+ba, ab+bb)

  -- reset the elements arg of all JoinCopies so they show up as total
  clearJoinComp (W.CGen c w)   = W.CGen c $ clearJoinWhat w
  clearJoinComp (W.CMap  w)    = W.CMap   $ clearJoinWhat w
  clearJoinComp (W.CFold w)    = W.CFold  $ clearJoinWhat w
  clearJoinComp (W.CScan w)    = W.CScan  $ clearJoinWhat w
  clearJoinComp (W.CDist w)    = W.CDist  $ clearJoinWhat w

  clearJoinWhat (W.WJoinCopy _)= W.WJoinCopy (-1)
  clearJoinWhat (W.WFMapMap p q) = W.WFMapMap (clearJoinWhat p) (clearJoinWhat q)
  clearJoinWhat (W.WFMapGen p q) = W.WFMapGen (clearJoinWhat p) (clearJoinWhat q)
  clearJoinWhat (W.WFZipMap p q) = W.WFZipMap (clearJoinWhat p) (clearJoinWhat q)
  clearJoinWhat w = w



pprDphOpsSumState :: DphOpsSumState -> Doc
pprDphOpsSumState (DphOpsSumState ops total unparse) = vcat [unparse', ops']
 where
  unparse'
        = if unparse == 0
          then text ""
          else text "Errors: " <> ppr unparse <> text " events unable to be parsed"

  ops' = vcat $ map pprOp $ sortBy cmp $ M.assocs ops
  cmp (_,(_,p)) (_,(_,q))
        = case compare p q of
          GT -> LT
          EQ -> EQ
          LT -> GT

  pprOp (c,(calls,duration)) = padLines (cat
        [ pprPercent duration
        , text " "
        , padR 10 (text "(" <> pprTimestampEng duration <> text ")")
        , text " "
        , padR 10 (ppr calls)
        , text " "
        ]) (show $ ppr c)
  pprPercent   v = padR 5  $ ppr (v * 100 `div` total) <> text "%"

instance Pretty W.Comp where
  ppr (W.CGen cheap what)
        = cheap' <> ppr what
        where
        cheap' = if cheap
                 then text "GenC "
                 else text "Gen  "
  ppr (W.CMap what)
        = text "Map  " <> ppr what
  ppr (W.CFold what)
        = text "Fold " <> ppr what
  ppr (W.CScan what)
        = text "Scan " <> ppr what
  ppr (W.CDist what)
        = text "Dist " <> ppr what

instance Pretty W.What where
  ppr (W.What str)            = text $ show str
  ppr (W.WScalar)          = text "Scalar"
  ppr (W.WZip)             = text "Zip"
  ppr (W.WSlice)           = text "Slice"
  ppr (W.WLength)          = text "Length"
  ppr (W.WLengthIdx)       = text "LengthIdx"
  ppr (W.WBpermute)        = text "Bpermute"
  ppr (W.WJoinCopy (-1))   = text  "JoinCp"
  ppr (W.WJoinCopy n)      = text ("JoinCp(" ++ show n ++ ")")
  ppr (W.WFMapMap p q) = text "(" <> ppr p <> text " mapMap " <> ppr q <> text ")"
  ppr (W.WFMapGen p q) = text "(" <> ppr p <> text " mapGen " <> ppr q <> text ")"
  ppr (W.WFZipMap p q) = text "(" <> ppr p <> text " zipMap " <> ppr q <> text ")"

chunks' len str
 = case chunks len str of
        (x:xs) -> (x:xs)
        []     -> [""]

chunks len [] = []
chunks len str
 = let (f,r) = splitAt len str
   in  f : chunks len r
