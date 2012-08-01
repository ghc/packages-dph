
module DPH.Pass.Dump
        (passDump)
where
import DPH.Core.Pretty
import HscTypes
import CoreMonad
import System.IO.Unsafe

-- | Dump a module.
passDump :: String -> ModGuts -> CoreM ModGuts
passDump name guts
 = unsafePerformIO
 $ do   let mdl = mg_module guts
        let binds = mg_binds guts 

        writeFile ("dump." ++ name ++ ".hs")
         $ render RenderIndent (pprTopBinds binds)

        return (return guts)
