import Distribution.Simple
import System.IO
main = defaultMainWithHooks
     $ simpleUserHooks {
         haddockHook = \_ _ _ _ -> warn
     }
  where
    warn = hPutStrLn stderr "WARNING: ndp not supported by hadock"

