module Testsuite.Preproc ( testcases )
where

import Language.Haskell.TH
import Data.List
import Monad (liftM)

data Prop = Prop { propName   :: Name
                 , propTyvars :: [Name]
                 , propType   :: Type
                 }

data Inst = Inst { instName   :: Name
                 , instSubsts :: [(Name, Type)]
                 , instExp    :: Exp
                 }

testcases :: Q Type -> Q [Dec] -> Q [Dec]
testcases qty qdecs = 
  do
    tys <- liftM types qty
    decs <- qdecs
    let props = embed . generate tys $ properties decs
        rn    = AppE (VarE (mkName "runTests"))
                     props
        main  = ValD (VarP (mkName "main"))
                     (NormalB rn) []
    return (decs ++ [main])

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

generate :: [Type] -> [Prop] -> [Inst]
generate tys = concatMap gen
  where
    gen prop@(Prop { propName   = name
                   , propTyvars = []
                   , propType   = ty }) =
          [Inst name [] (VarE name `SigE` ty)]
    gen prop@(Prop { propName   = name
                   , propTyvars = tvs
                   , propType   = ty }) =
          [Inst name env (VarE name `SigE` subst env ty)
           | env <- combinations tvs tys]

subst :: [(Name, Type)] -> Type -> Type
subst env (VarT nm)  = case lookup nm env of
                         Just ty -> ty
subst env (AppT t u) = AppT (subst env t) (subst env u)
subst env t          = t

combinations :: [a] -> [b] -> [[(a,b)]]
combinations []     _  = []
combinations _      [] = []
combinations [x]    ys = [[(x,y)] | y <- ys]
combinations (x:xs) ys = [(x,y) : ps | y <- ys, ps <- combinations xs ys]

