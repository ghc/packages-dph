

module DPH.Plugin where
import GhcPlugins
import StaticFlags
import Debug.Trace
import System.IO.Unsafe

plugin :: Plugin
plugin  = defaultPlugin 
        { installCoreToDos = install }


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos 
 = do   
        -- Initialize the staticflags so that we can pretty print core code.
        --   The pretty printers depend on static flags and will `error` if 
        --   we don't do this first.
        unsafePerformIO
         $ do   initStaticOpts
                return (return ())

        -- Flatten out the tree of passes into a list to make it easier to handle.
        let todos' = normalizeCoreDoPasses todos

        -- Check if this module is being vectorised.
        let hasVectorisation
                   = any isCoreDoVectorisation todos'

        -- If the vectoriser is enabled then replace the standard GHC pipeline
        -- with our own.
        if hasVectorisation
           then return vectoriserPipeline
           else return todos'


dphPhaseClosure = 4

-- | Our vectoriser pipeline.
--   This replaces the standard compilation pipeline defined in 
--   SimplCore.lhs of the main compiler.
vectoriserPipeline :: [CoreToDo]
vectoriserPipeline
 = [    -- The vectoriser requires the desugared code to be pre-simplified
        CoreDoSimplify 10
                SimplMode 
                { sm_names      = ["Vectorise", "PreSimplify"]
                , sm_phase      = InitialPhase
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = False
                , sm_case_case  = False }

        -- Run the vectoriser.
   ,    CoreDoVectorisation 

        ---------------------
        -- We inline the different combinator layers one after another,
        -- to give their associated rewrite rules a chance to fire.
        --   Each phase reduces the abstraction level of our code.

        -- Inline lifted applications and closure constructors.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Closures"]
                , sm_phase      = Phase 4
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

        -- Inline PArray and PData combinators.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "PArray"]
                , sm_phase      = Phase 3
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

        -- Inline unlifted backend.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Unlifted"]
                , sm_phase      = Phase 2
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

        -- Inline stream functions.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Stream"]
                , sm_phase      = Phase 1
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

        -- Inline inner loops and everything else.
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Stream"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True } 

        ---------------------
        -- We've got everything expressed as loops, 
        -- and need to optimise this low level code.

   ,    CoreDoStrictness

   ,    CoreDoWorkerWrapper

   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post Worker-Wrapper"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }

   ,    CoreDoSpecConstr
   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Post SpecConstr"]
                , sm_phase      = Phase 0
                , sm_rules      = False
                , sm_eta_expand = False
                , sm_inline     = False
                , sm_case_case  = True }


   ,    CoreDoFloatOutwards
                FloatOutSwitches
                { floatOutLambdas               = Nothing
                , floatOutConstants             = False 
                , floatOutPartialApplications   = False }

   ,    CoreCSE

   ,    CoreDoFloatInwards

   ,    CoreDoSimplify 10
                SimplMode
                { sm_names      = ["Vectorise", "Final"]
                , sm_phase      = Phase 0
                , sm_rules      = True
                , sm_eta_expand = True
                , sm_inline     = True
                , sm_case_case  = True }
   ]
                 

-- CoreToDo -------------------------------------------------------------------
-- | Flatten `CoreDoPasses` and drop out `CoreDoNothing` const
normalizeCoreDoPasses :: [CoreToDo] -> [CoreToDo]
normalizeCoreDoPasses cc
 = case cc of
        [] -> []

        CoreDoPasses cs1 : cs2
           -> normalizeCoreDoPasses (cs1 ++ cs2)

        CoreDoNothing : cs
           -> normalizeCoreDoPasses cs

        c : cs
           -> c : normalizeCoreDoPasses cs


-- | Check if a `CoreToDo` is `CoreDoVectorisation`
isCoreDoVectorisation :: CoreToDo -> Bool
isCoreDoVectorisation cc
 = case cc of
        CoreDoVectorisation     -> True
        _                       -> False


-- The Plugin -----------------------------------------------------------------
-- | The main DPH Plugin.
dphPluginPass :: PluginPass
dphPluginPass modGuts
        = trace "******** PASS"
        $ dumpCore modGuts

dumpCore :: ModGuts -> CoreM ModGuts 
dumpCore guts
 = unsafePerformIO
 $ do   putStrLn $ "*** GUTS " ++ show (length $ mg_binds guts)

        return (return guts)

