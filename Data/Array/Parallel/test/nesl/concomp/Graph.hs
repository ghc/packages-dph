module Graph
where

import Data.Array.Parallel.Unlifted

data Graph = Graph { nodeCount :: Int
                   , edgeCount :: Int
                   , edges     :: UArr (Int :*: Int)
                   }
  deriving(Read,Show)

