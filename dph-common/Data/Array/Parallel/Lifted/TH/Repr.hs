{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Array.Parallel.Lifted.TH.Repr (
  tupleInstances
) where

import Data.Array.Parallel.Lifted.PArray

import Language.Haskell.TH
import Data.List (transpose)

pdataTupCon :: Int -> Name
pdataTupCon n = mkName ("P_" ++ show n)

varTs = map varT
appTs = foldl appT

varEs = map varE
appEs = foldl appE

patLetE pat exp body = letE [valD pat (normalB exp) []] body

varPs = map varP

vanillaC con tys = normalC con (map (strictType notStrict) tys)

simpleFunD name pats exp
  = funD name [clause pats (normalB exp) []]

inlineD :: Name -> DecQ
inlineD name = pragInlD name (inlineSpecNoPhase True False)

instance_PData :: TypeQ -> [Name] -> Name -> [TypeQ] -> DecQ
instance_PData tycon tyargs con tys
  = dataInstD (cxt []) ''PData [tycon `appTs` varTs tyargs]
                               [vanillaC con tys]
                               []


tupleInstances :: [Int] -> Q [Dec]
tupleInstances ns
  = do
      pdatas <- mapM instance_PData_tup ns
      prs    <- mapM instance_PR_tup ns
      return $ pdatas ++ prs

instance_PData_tup :: Int -> DecQ
instance_PData_tup arity
  = instance_PData (tupleT arity) vars (pdataTupCon arity)
                [conT ''PData `appT` varT v | v <- vars]
  where
    vars = take arity $ [mkName [c] | c <- ['a' .. ]]


instance_PR_tup :: Int -> DecQ
instance_PR_tup arity
  = instanceD (cxt [classP ''PR [ty] | ty <- tys])
              (conT ''PR `appT` (tupleT arity `appTs` tys))
              (map (inlineD . mkName . fst) methods ++ map snd methods)
  where
    tyvars = take arity $ [mkName [c] | c <- ['a' .. ]]
    tys    = map varT tyvars

    pcon   = pdataTupCon arity

    pconApp = appEs (conE pcon)
    pconPat = conP pcon

    vars   = take arity $ [mkName [c] | c <- ['a' .. ]]
    pvars  = take arity $ [mkName (c : "s") | c <- ['a' .. ]]
    pvars1 = take arity $ [mkName (c : "s1") | c <- ['a' .. ]]
    pvars2 = take arity $ [mkName (c : "s2") | c <- ['a' .. ]]

    methods = [ ("emptyPR",             m_empty                 )
              , ("replicatePR",         m_replicate             )
              , ("replicatelPR",        m_replicatel            )
              , ("repeatPR",            m_repeat                )
              , ("repeatcPR",           m_repeatc               )
              , ("indexPR",             m_index                 )
              , ("extractPR",           m_extract               )
              , ("bpermutePR",          m_bpermute              )
              , ("appPR",               m_app                   )
              , ("applPR",              m_appl                  )
              , ("packPR",              m_pack                  )
              , ("packByTagPR",         m_packByTag             )
              , ("combine2PR",          m_combine2              )
              , ("fromListPR",          m_fromList              )
              , ("nfPR",                m_nf                    )
              ]

    method :: String -> [String] -> [[Name]]
            -> ([PatQ] -> PatQ) -> ([ExpQ] -> ExpQ)
            -> (forall a. [a] -> [a] -> [a]) -> DecQ
    method s args vs mk_pat mk_con insert 
      = simpleFunD (mkName s)
                   (insert (varPs args') (map (mk_pat . varPs) vs))
        $ mk_con 
            [varE (mkName s) `appEs` insert (varEs args') (map varE v)
                | v <- transpose vs]
      where
        args' = map mkName args

    m_empty = simpleFunD (mkName "emptyPR") []
            $ conE pcon `appEs` replicate arity (varE 'emptyPR)

    m_replicate = method "replicatePR" ["n#"] [vars] tupP pconApp
                $ \[n] [x] -> [n,x]

    m_replicatel = method "replicatelPR" ["segd"] [pvars] pconPat pconApp
                $ \[segd] [x] -> [segd,x]

    m_repeat = method "repeatPR" ["n#", "len#"] [pvars] pconPat pconApp
                $ \[n,len] [x] -> [n,len,x]

    m_repeatc = method "repeatcPR" ["n#","ns","segd"] [pvars] pconPat pconApp
                $ \[n,ns,segd] [x] -> [n,ns,segd,x]

    m_index = method "indexPR" ["i#"] [pvars] pconPat tupE
                $ \[i] [x] -> [x,i]

    m_extract = method "extractPR" ["i#","n#"] [pvars] pconPat pconApp
                $ \[i,n] [x] -> [x,i,n]

    m_bpermute = method "bpermutePR" ["n#","is"] [pvars] pconPat pconApp
                $ \[n,is] [x] -> [x,n,is]

    m_app = method "appPR" [] [pvars1, pvars2] pconPat pconApp
                $ \[] [x,y] -> [x,y]

    m_appl = method "applPR" ["is","js"] [pvars1, pvars2] pconPat pconApp
                $ \[is,js] [x,y] -> [is,x,js,y]

    m_pack = method "packPR" ["n#","sel"] [pvars] pconPat pconApp
                $ \[n,sel] [x] -> [x,n,sel]

    m_packByTag = method "packByTagPR" ["n#","tags","t#"] [pvars] pconPat pconApp
                $ \[n,tags,t] [x] -> [x,n,tags,t]

    m_combine2 = method "combine2PR" ["n#","sel"] [pvars1,pvars2] pconPat pconApp
                $ \[n,sel] [x,y] -> [n,sel,x,y]

    m_fromList = method "fromListPR" ["n#"] [pvars] (const $ varP xs) mk_body
                $ \[n] [x] -> [n,x]
      where
        mk_body = patLetE (tupP $ varPs pvars) (varE unzip `appE` varE xs)
                . pconApp

        xs = mkName "xs"

        unzip | arity == 2 = mkName "unzip"
              | otherwise  = mkName ("unzip" ++ show arity)

    m_nf = method "nfPR" [] [pvars] pconPat mk_body
                $ \[] [x] -> [x]
      where
        mk_body = foldl1 (\e1 e2 -> varE 'seq `appEs` [e1,e2])

