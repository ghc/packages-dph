
module Data.Array.Parallel.Unlifted.Distributed.What
        ( Comp  (..)
        , What  (..))
where
        


-- | What distributed computation we are doing.
data Comp
        = CompGen       { compCheap     :: Bool
                        , compWhat      :: What}

        | CompMap       { compWhat      :: What }
        | CompFold      { compWhat      :: What }
        | CompScan      { compWhat      :: What }
        | CompDist      What
        deriving Show

-- | What sort of thing is being computed.
data What
        = What            String
        | WhatScalar 
        | WhatZip
        | WhatSlice
        | WhatLength
        | WhatLengthIdx
        | WhatBpermute

        -- Copy due to a join instruction.
        | WhatJoinCopy  { whatElems     :: Int }

        | WhatFusedMapMap What What
        | WhatFusedMapGen What What
        | WhatFusedZipMap What What
        deriving Show
