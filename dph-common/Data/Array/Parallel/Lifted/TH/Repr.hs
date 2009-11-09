{-# LANGUAGE TemplateHaskell, Rank2Types #-}
module Data.Array.Parallel.Lifted.TH.Repr (
  primInstances, tupleInstances, voidPRInstance, unitPRInstance
) where

import qualified Data.Array.Parallel.Unlifted as U
import Data.Array.Parallel.Lifted.PArray

import Language.Haskell.TH
import Data.List (transpose)

tyBndrVar :: TyVarBndr -> Name
tyBndrVar (PlainTV  n)   = n
tyBndrVar (KindedTV n _) = n

mkAppTs = foldl AppT

varTs = map varT
appTs = foldl appT

varEs = map varE
appEs = foldl appE

patLetE pat exp body = letE [valD pat (normalB exp) []] body
normalMatch pat exp = match pat (normalB exp) []

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

newtype_instance_PData :: Name -> [Name] -> Name -> TypeQ -> DecQ
newtype_instance_PData tycon tyargs con ty
  = newtypeInstD (cxt []) ''PData [conT tycon `appTs` varTs tyargs]
                                  (vanillaC con [ty])
                                  []

splitConAppTy :: Type -> Maybe (Type, [Type])
splitConAppTy ty = collect ty []
  where
    collect (ConT tycon)  args = Just (ConT tycon, args)
    collect (TupleT n)    args = Just (TupleT n,   args)
    collect ArrowT        args = Just (ArrowT,     args)
    collect (AppT ty arg) args = collect ty (arg:args)
    collect _ _ = Nothing

normaliseTy :: Type -> Q Type
normaliseTy ty
  = case splitConAppTy ty of
      Just (ConT tycon, args)
        -> do
             info <- reify tycon
             case info of
               TyConI (TySynD _ bndrs ty)
                 -> return $ substTy (zip (map tyBndrVar bndrs) args) ty
               _ -> return ty
      _ -> return ty

substTy :: [(Name, Type)] -> Type -> Type
substTy env (ForallT _ _ _) = error "DPH gen: can't substitute in forall ty"
substTy env (VarT v)   = case lookup v env of
                           Just ty -> ty
                           Nothing -> VarT v
substTy env (AppT t u) = AppT (substTy env t) (substTy env u)
substTy env (SigT t k) = SigT (substTy env t) k
substTy env t          = t

splitFunTy :: Type -> ([Type], Type)
splitFunTy ty = case splitConAppTy ty of
                  Just (ArrowT, [arg, r]) -> let (args, res) = splitFunTy r
                                             in (arg:args, res)
                  _ -> ([], ty)

data Val = ScalarVal
         | PDataVal
         | ListVal
         | UnitVal
         | OtherVal
type NameGen = String -> String
type ArgVal = (Val, NameGen)

genPR_methods :: (Name -> [ArgVal] -> Val -> DecQ) -> Q [Dec]
genPR_methods mk_method
  = do
      ClassI (ClassD _ _ _ _ decs) <- reify ''PR
      inls <- sequence [inlineD $ mkName $ nameBase name | SigD name _ <- decs]
      defs <- mapM gen [(name, ty) | SigD name ty <- decs]
      return $ inls ++ defs
  where
    gen (name, ty)
      = case lookup name nameGens of
          Just gs -> do
                       (args, res) <- methodVals ty
                       mk_method name (zip args gs) res
          Nothing -> error $ "DPH gen: no name generator for " ++ show name

methodVals :: Type -> Q ([Val], Val)
methodVals (ForallT (PlainTV v : _) _ ty)
  = do
      ty' <- normaliseTy ty
      let (args, res) = splitFunTy ty'
      return (map (val v) args, val v res)
  where
    val v (VarT n) | v == n = ScalarVal
    val v (AppT (ConT c) (VarT n)) | c == ''PData && v == n = PDataVal
                                   | c == ''[]    && v == n = ListVal
    val v (ConT c) | c == ''() = UnitVal
    val _ t = OtherVal

data Split = PatSplit  PatQ
           | CaseSplit PatQ ExpQ PatQ

data Arg = RecArg   [ExpQ] [ExpQ]
         | OtherArg ExpQ

data Gen = Gen {
             recursiveCalls :: Int
           , split          :: ArgVal -> (Split, Arg)
           , join           :: Name -> [Arg] -> Val -> [ExpQ] -> ExpQ
           }

recursiveMethod :: Gen -> Name -> [ArgVal] -> Val -> DecQ
recursiveMethod gen meth avs res
  = simpleFunD (mkName $ nameBase meth) (map pat splits)
  $ foldr mk_case
    (join gen meth args res
     . recurse (recursiveCalls gen)
     . trans
     $ map expand args)
    splits
  where
    (splits, args) = unzip (map split_arg avs)

    pat (PatSplit  p)     = p
    pat (CaseSplit p _ _) = p

    split_arg (OtherVal,  g) = let v = mkName (g "")
                               in
                               (PatSplit (varP v), OtherArg (varE v))
    split_arg arg = split gen arg

    mk_case (PatSplit  _)           exp = exp
    mk_case (CaseSplit _ scrut pat) exp = caseE scrut [normalMatch pat exp]

    expand (RecArg _ es) = es
    expand (OtherArg  e) = repeat e

    trans [] = []
    trans [xs] = [[x] | x <- xs]
    trans (xs : yss) = zipWith (:) xs (trans yss)

    recurse 0 _ = []
    recurse n [] = replicate n (varE meth)
    recurse n args = [varE meth `appEs` es| es <- take n args]

nameGens =
  [
    ('emptyPR,          [])
  , ('replicatePR,      [const "n#", id])
  , ('replicatelPR,     [const "segd", id])
  , ('repeatPR,         [const "n#", const "len#", id])
  , ('repeatcPR,        [const "n#", const "ns", const "segd", id])
  , ('indexPR,          [id, const "i#"])
  , ('extractPR,        [id, const "i#", const "n#"])
  , ('bpermutePR,       [id, const "n#", const "is"])
  , ('appPR,            [(++"1"), (++"2")])
  , ('applPR,           [const "is", (++"1"), const "js", (++"2")])
  , ('packPR,           [id, const "n#", const "sel"])
  , ('packByTagPR,      [id, const "n#", const "tags", const "t#"])
  , ('combine2PR,       [const "n#", const "sel", (++"1"), (++"2")])
  , ('fromListPR,       [const "n#", id])
  , ('nfPR,             [id])
  ]

-- ---------------
-- Primitive types
-- ---------------

primInstances :: [Name] -> Q [Dec]
primInstances tys
  = do
      pdatas <- mapM instance_PData_prim tys
      prims  <- mapM instance_Prim_prim tys
      prs    <- mapM instance_PR_prim tys
      return $ pdatas ++ prims ++ prs

pdataPrimCon :: Name -> Name
pdataPrimCon n = mkName ("P" ++ nameBase n)

instance_PData_prim :: Name -> DecQ
instance_PData_prim tycon
  = newtype_instance_PData tycon [] (pdataPrimCon tycon)
                                    (conT ''U.Array `appT` conT tycon)

instance_Prim_prim :: Name -> DecQ
instance_Prim_prim ty
  = instanceD (cxt [])
              (conT ''Prim `appT` conT ty)
              (map (inlineD . mkName . fst) methods ++ map snd methods)
  where
    pcon = pdataPrimCon ty
    xs   = mkName "xs"

    methods = [("fromPrimPData", mk_fromPrimPData),
               ("toPrimPData",   mk_toPrimPData)]

    mk_fromPrimPData = simpleFunD (mkName "fromPrimPData")
                                  [conP pcon [varP xs]]
                                  (varE xs)
    mk_toPrimPData = simpleFunD (mkName "toPrimPData") [] (conE pcon)

instance_PR_prim :: Name -> DecQ
instance_PR_prim ty
  = do
      methods <- genPR_methods (primitiveMethod ty)
      return $ InstanceD []
                         (ConT ''PR `AppT` ConT ty)
                         methods

primitiveMethod :: Name -> Name -> [ArgVal] -> Val -> DecQ
primitiveMethod ty meth avs res
  = simpleFunD (mkName $ nameBase meth) []
  $ varE
  $ mkName (nameBase meth ++ "Prim")

{-
  = simpleFunD (mkName $ nameBase meth) pats
  $ result res
  $ varE impl `appEs` vals
  where
    pcon = pdataPrimCon ty
    impl = mkName
         $ nameBase meth ++ "Prim"

    (pats, vals) = unzip [arg v g | (v,g) <- avs]

    arg ScalarVal g = var (g "x")
    arg PDataVal  g = let v = mkName (g "xs")
                      in (conP pcon [varP v], varE v)
    arg ListVal   g = var (g "xs")
    arg OtherVal  g = var (g "")

    var s = let v = mkName s in (varP v, varE v)

    result ScalarVal e = e
    result PDataVal  e = conE pcon `appE` e
    result UnitVal   e = varE 'seq `appEs` [e, varE '()]
    result OtherVal  e = e
-}

-- ----
-- Void
-- ----

voidPRInstance :: Name -> Name -> Name -> Q [Dec]
voidPRInstance ty void pvoid
  = do
      methods <- genPR_methods (voidMethod void pvoid)
      return [InstanceD []
                        (ConT ''PR `AppT` ConT ty)
                        methods]

voidMethod :: Name -> Name -> Name -> [ArgVal] -> Val -> DecQ
voidMethod void pvoid meth avs res
  = simpleFunD (mkName $ nameBase meth) (map (const wildP) avs)
  $ result res
  where
    result ScalarVal = varE void
    result PDataVal  = varE pvoid
    result UnitVal   = conE '()

-- --
-- ()
-- --

unitPRInstance :: Name -> Q [Dec]
unitPRInstance punit
  = do
      methods <- genPR_methods (unitMethod punit)
      return [InstanceD []
                        (ConT ''PR `AppT` ConT ''())
                        methods]

unitMethod :: Name -> Name -> [ArgVal] -> Val -> DecQ
unitMethod punit meth avs res
  = simpleFunD (mkName $ nameBase meth) pats
  $ foldr seq_val (result res) es
  where
    (pats, es) = unzip [pat v g | (v,g) <- avs]

    pat ScalarVal _ = (conP '() [], Nothing)
    pat PDataVal  _ = (conP punit [], Nothing)
    pat ListVal   g = let xs = mkName (g "xs")
                      in
                      (varP xs, Just $
                        \e -> varE 'foldr `appEs` [varE 'seq, e, varE xs])
    pat OtherVal  _ = (wildP, Nothing)

    result ScalarVal = conE '()
    result PDataVal  = conE punit
    result UnitVal   = conE '()

    seq_val Nothing  e = e
    seq_val (Just f) e = f e



-- ------
-- Tuples
-- ------

tupleInstances :: [Int] -> Q [Dec]
tupleInstances ns
  = do
      pdatas <- mapM instance_PData_tup ns
      prs    <- mapM instance_PR_tup ns
      return $ pdatas ++ prs

pdataTupCon :: Int -> Name
pdataTupCon n = mkName ("P_" ++ show n)

instance_PData_tup :: Int -> DecQ
instance_PData_tup arity
  = instance_PData (tupleT arity) vars (pdataTupCon arity)
                [conT ''PData `appT` varT v | v <- vars]
  where
    vars = take arity $ [mkName [c] | c <- ['a' .. ]]


instance_PR_tup :: Int -> DecQ
instance_PR_tup arity
  = do
      methods <- genPR_methods (recursiveMethod (tupGen arity))
      return $ InstanceD [ClassP ''PR [ty] | ty <- tys]
                         (ConT ''PR `AppT` (TupleT arity `mkAppTs` tys))
                         methods
  where
    tys = take arity $ [VarT $ mkName [c] | c <- ['a' .. ]]

tupGen :: Int -> Gen
tupGen arity = Gen { recursiveCalls = arity
                   , split          = split
                   , join           = join }
  where
    split (ScalarVal, gen)
      = (PatSplit (tupP $ varPs names), RecArg [] (varEs names))
      where
        names = map (mkName . gen) vs

    split (PDataVal, gen)
      = (PatSplit (conP (pdataTupCon arity) $ varPs names),
         RecArg [] (varEs names))
      where
        names = map (mkName . gen) pvs

    split (ListVal, gen)
      = (CaseSplit (varP xs) (varE unzip `appE` varE xs)
                             (tupP $ varPs names),
         RecArg [] (varEs names))
      where
        xs = mkName (gen "xs")
        names = map (mkName . gen) pvs

        unzip | arity == 2 = mkName "unzip"
              | otherwise  = mkName ("unzip" ++ show arity)

    join _ _ ScalarVal xs = tupE xs
    join _ _ PDataVal  xs = conE (pdataTupCon arity) `appEs` xs
    join _ _ UnitVal   xs = foldl1 (\x y -> varE 'seq `appEs` [x,y]) xs

    vs  = take arity [[c] | c <- ['a' ..]]
    pvs = take arity [c : "s" | c <- ['a' ..]]


