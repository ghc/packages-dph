
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

instance PprPhysical Int where
 pprp = text . show 
 
instance PprPhysical Double where
 pprp = text . show 
 
 
-- | Pretty print virtual \/ logical structure of data.
class PprVirtual a where
 pprv :: a -> Doc

instance PprVirtual Int where
 pprv = text . show 
 
instance PprVirtual Double where
 pprv = text . show 

 