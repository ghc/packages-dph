module Testsuite.Preproc ( testcases, (<@) )
where

import Language.Haskell.TH
import Data.List
import Data.Maybe (fromJust)
import Monad (liftM)

data Prop = Prop { propName   :: Name
                 , propTyvars :: [Name]
                 , propType   :: Type
                 }

data Inst = Inst { instName   :: Name
                 , instSubsts :: [(Name, Type)]
                 , instExp    :: Exp
                 }

(<@) :: String -> Q Type -> Q (String, Type)
pfx <@ qty = liftM ((,) pfx) qty

type Domain = [(String, [Type])]

testcases :: [Q (String, Type)] -> Q [Dec] -> Q [Dec]
testcases qdom qdecs =
  do
    dom <- liftM domain $ sequence qdom
    decs <- qdecs
    let props = embed . generate dom $ properties decs
        rn    = AppE (VarE (mkName "runTests"))
                     props
        main  = ValD (VarP (mkName "main"))
                     (NormalB rn) []
    return (decs ++ [main])

domain :: [(String, Type)] -> Domain
domain ps = sortBy cmpPfx
          . zip (map fst ps)
          . map types
          $ map snd ps
  where
    cmpPfx (s,_) (s',_) = length s' `compare` length s

types :: Type -> [Type]
types ty = case unAppT ty of
             (TupleT _ : tys) -> tys
             _                -> [ty]
  where
    unAppT (AppT t u) = unAppT t ++ [u]
    unAppT t          = [t]


instid :: Inst -> String
instid inst = name inst ++ env inst
  where
    name (Inst { instName = nm }) =
      let s = nameBase nm
      in
      if "prop_" `isPrefixOf` s then drop 5 s else s

    env (Inst { instSubsts = substs })
      | null substs = ""
      | otherwise   = let ss = [nameBase tv ++ " = " ++ pprint ty
                                | (tv, ty) <- substs]
                      in "[" ++ head ss ++ concatMap (", " ++) (tail ss) ++ "]"

properties :: [Dec] -> [Prop]
properties decs = [mkProp nm ty | SigD nm ty <- decs]
  where
    mkProp nm (ForallT vars _ ty) = Prop nm vars ty
    mkProp nm ty                  = Prop nm []   ty
                         
embed :: [Inst] -> Exp
embed insts = ListE [((VarE $ mkName "mkTest")    `AppE`
                     (LitE . StringL $ instid i)) `AppE`
                     instExp i
                    | i <- insts ]

generate :: Domain -> [Prop] -> [Inst]
generate dom = concatMap gen
  where
    gen prop@(Prop { propName   = name
                   , propTyvars = []
                   , propType   = ty }) =
          [Inst name [] (VarE name `SigE` ty)]
    gen prop@(Prop { propName   = name
                   , propTyvars = tvs
                   , propType   = ty }) =
          [Inst name env (VarE name `SigE` subst env ty)
           | env <- combinations tvs dom]

subst :: [(Name, Type)] -> Type -> Type
subst env (VarT nm)  = case lookup nm env of
                         Just ty -> ty
subst env (AppT t u) = AppT (subst env t) (subst env u)
subst env t          = t

combinations :: [Name] -> [(String, [Type])] -> [[(Name, Type)]]
combinations []     _   = [[]]
combinations (n:ns) dom = [(n,t) : ps | t <- ts, ps <- combinations ns dom]
  where
    s  = nameBase n
    ts = snd . fromJust $ find ((`isPrefixOf` s) . fst) dom

