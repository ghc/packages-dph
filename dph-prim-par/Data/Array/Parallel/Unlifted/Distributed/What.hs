
module Data.Array.Parallel.Unlifted.Distributed.What
        ( Comp  (..)
        , What  (..))
where
        


-- | What computation we are doing.
data Comp
        = CompGenerate
                { compCheap     :: Bool
                , compWhat      :: What}

        | CompMap
                { compWhat      :: What }
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

        | WhatFusedMapMap What What
        | WhatFusedMapGen What What
        | WhatFusedZipMap What What
        deriving Show
