
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
        = validPA pdata
 
  emptyPR               
        = PWrap emptyPA
  
  nfPR (PWrap pdata)      
        = nfPA pdata
        
  lengthPR (PWrap pdata)
        = lengthPA pdata
        
  replicatePR n (Wrap x)
        = PWrap $ replicatePA n x

  replicatesPR segd (PWrap xs)
        = PWrap $ replicatesPA segd xs

  indexPR (PWrap xs) ix
        = Wrap  $ indexPA xs ix

  -- PROBLEM: unwrapping isn't O(1).
  indexlPR n (PNested vsegd pdatas) ixs
   = let pdatas' = V.map (\(PWrap a) -> a) pdatas
     in  PWrap (indexlPA n (PNested vsegd pdatas') ixs)

  extractPR (PWrap xs) ix n
        = PWrap $ extractPA xs ix n
        
  -- PROBLEM: unwrapping isn't O(1).
  extractsPR vecs ssegd
        = PWrap $ extractsPA (V.map (\(PWrap a) -> a) vecs) ssegd

  appendPR (PWrap xs) (PWrap ys)
        = PWrap $ appendPA xs ys
        
  appendsPR segdResult segd1 (PWrap xs) segd2 (PWrap ys)
        = PWrap $ appendsPA segdResult segd1 xs segd2 ys
        
  packByTagPR (PWrap xs) tags tag
        = PWrap $ packByTagPA xs tags tag

  combine2PR sel (PWrap xs) (PWrap ys)
        = PWrap $ combine2PA sel xs ys

  fromVectorPR vec 
        = PWrap $ fromVectorPA $ V.map unWrap vec
        
  toVectorPR (PWrap pdata)
        = V.map Wrap $ toVectorPA pdata
