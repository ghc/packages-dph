import Distribution.Simple
import System.IO
main = defaultMainWithHooks
     $ defaultUserHooks {
         haddockHook = \_ _ _ _ -> warn
     }
  where
    warn = hPutStrLn stderr "WARNING: ndp not supported by hadock"

