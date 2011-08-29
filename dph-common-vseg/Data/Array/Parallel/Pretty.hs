
-- | Pretty printer classes
module Data.Array.Parallel.Pretty
        ( module Text.PrettyPrint
        , PprPhysical(..)
        , PprVirtual (..))
where
import Text.PrettyPrint


-- | Pretty print physical structure of data.
class PprPhysical a where
 pprp :: a -> Doc
 
 
-- | Pretty print virtual / logical structure of data.
class PprVirtual a where
 pprv :: a -> Doc

 
 