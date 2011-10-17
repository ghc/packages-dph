
module Data.Array.Parallel.PArray.PData.Wrap where
import Data.Array.Parallel.PArray.PData.Base
import Data.Array.Parallel.PArray.PData.Nested
import Data.Array.Parallel.PArray.Types
import Data.Array.Parallel.PArray.PRepr.Base
import Data.Vector.Unboxed                      (Vector)
import qualified Data.Vector                    as V

newtype instance PData (Wrap a)
        = PWrap (PData a)

instance PA a => PR (Wrap a) where       

  validPR (PWrap pdata)  
        = validPD pdata
 
  emptyPR               
        = PWrap emptyPD
  
  nfPR (PWrap pdata)      
        = nfPD pdata
        
  lengthPR (PWrap pdata)
        = lengthPD pdata
        
  replicatePR n (Wrap x)
        = PWrap $ replicatePD n x

  replicatesPR segd (PWrap xs)
        = PWrap $ replicatesPD segd xs

  indexPR (PWrap xs) ix
        = Wrap  $ indexPD xs ix

  -- PROBLEM: unwrapping isn't O(1).
  indexlPR n (PNested vsegd pdatas) ixs
   = let pdatas' = V.map (\(PWrap a) -> a) pdatas
     in  PWrap (indexlPD n (PNested vsegd pdatas') ixs)

  extractPR (PWrap xs) ix n
        = PWrap $ extractPD xs ix n
        
  -- PROBLEM: unwrapping isn't O(1).
  extractsPR vecs ssegd
        = PWrap $ extractsPD (V.map (\(PWrap a) -> a) vecs) ssegd

  appendPR (PWrap xs) (PWrap ys)
        = PWrap $ appendPD xs ys
        
  appendsPR segdResult segd1 (PWrap xs) segd2 (PWrap ys)
        = PWrap $ appendsPD segdResult segd1 xs segd2 ys
        
  packByTagPR (PWrap xs) tags tag
        = PWrap $ packByTagPD xs tags tag

  combine2PR sel (PWrap xs) (PWrap ys)
        = PWrap $ combine2PD sel xs ys

  fromVectorPR vec 
        = PWrap $ fromVectorPD $ V.map unWrap vec
        
  toVectorPR (PWrap pdata)
        = V.map Wrap $ toVectorPD pdata
