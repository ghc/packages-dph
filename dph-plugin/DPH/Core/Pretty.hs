
module DPH.Core.Pretty 
        ( module DPH.Base.Pretty
        , pprTopBinds)
where
import DPH.Base.Pretty
import CoreSyn
import Type
import Coercion
import Var
import Name
import OccName
import DataCon
import Literal


-- Top Binds ------------------------------------------------------------------
pprTopBinds :: Pretty a => [Bind a] -> Doc
pprTopBinds binds
        = vcat $ map pprTopBind binds

pprTopBind  :: Pretty a => Bind a -> Doc
pprTopBind (NonRec binder expr)
  =    pprBinding (binder, expr) 
  <$$> empty

pprTopBind (Rec [])
  = text "Rec { }"

pprTopBind (Rec bb@(b:bs))
  = vcat 
  [ text "Rec {"
  , vcat [empty <$$> pprBinding b | b <- bb]
  , text "end Rec }"
  , empty ]


-- Binding --------------------------------------------------------------------
pprBinding :: Pretty a => (a, Expr a) -> Doc
pprBinding (binder, x)
        =   ppr binder
        <+> breakWhen (not $ isSimpleX x)
        <+> equals <+> align (ppr x)
              



-- Expr -----------------------------------------------------------------------
instance Pretty a => Pretty (Expr a) where
 pprPrec d xx
  = case xx of
        Var  name       -> ppr name
        Type _          -> empty        -- Discard Types
        Coercion _      -> empty        -- Discard Coercions
        Lit ll          -> ppr ll

        Cast x _co
         -> pprPrec d x
--         pprParen' (d > 10)
--         $  pprPrec 11 x <+> text "`cast`" <+> text "..."

        Lam{}
         -> pprParen' (d > 2)
         $  let (bndrs, body) = collectBinders xx
            in  text "\\" <> sep (map ppr bndrs)
                 <> text "." 
                 <> (nest 2 
                        $ (breakWhen $ not $ isSimpleX body)
                         <> ppr body)

        App x1 x2
         |  isTypeArg x2
         -> pprPrec d x1

         |  otherwise
         -> pprParen' (d > 10)
         $  ppr x1
                <> nest 4 (breakWhen (not $ isSimpleX x2) 
                                <> pprPrec 11 x2)

        Case x1 var ty [(con, binds, x2)]
         -> pprParen' (d > 2)
         $  text "let" 
                <+> (fill 12 (ppr con <+> hsep (map ppr binds)))
--                <>  breakWhen (not $ isSimpleX x1)
                        <>  text "<-"
                        <+> ppr x1
                        <+> text "in"
                <$$> ppr x2

        Case x1 var ty alts
         -> pprParen' (d > 2)
         $  (nest 2 
                $ text "case" <+> ppr x1 <+> text "of" 
                <+> ppr var
                <+> lbrace <> line
                        <> vcat (punctuate semi $ map pprAlt alts))
         <>  line <> rbrace


        Let (NonRec b x1) x2
         -> pprParen' (d > 2)
         $  text "let" 
                <+> fill 12 (ppr b)
                <+> equals 
                <+> ppr x1 
                <+> text "in" 
                <$$> ppr x2

        Let (Rec bxs) x2
          -> text "LETREC"

        _ -> text "DUNNO"


-- Alt ------------------------------------------------------------------------
pprAlt :: Pretty a => (AltCon, [a], Expr a) -> Doc
pprAlt (con, binds, x)
        = ppr con <+> (hsep $ map ppr binds) 
        <+> nest 1 (line <> nest 3 (text "->" <+> ppr x))

instance Pretty AltCon where
 ppr con
  = case con of
        DataAlt con     -> ppr con
        LitAlt  lit     -> ppr lit
        DEFAULT         -> text "_"


-- Literal --------------------------------------------------------------------
instance Pretty Literal where
 ppr _  = text "<LITERAL>"


-- Type -----------------------------------------------------------------------
instance Pretty Type where
 ppr _  = empty


-- Coercion -------------------------------------------------------------------
instance Pretty Coercion where
 ppr _  = empty


-- Names ----------------------------------------------------------------------
instance Pretty DataCon where
 ppr con 
        = ppr (dataConName con)


instance Pretty CoreBndr where
 ppr bndr
        = ppr (Var.varName bndr)


instance Pretty Name where
 ppr name
        = ppr (nameOccName name)

instance Pretty OccName where
 ppr occName
        = text (occNameString occName)



-- Utils ----------------------------------------------------------------------
breakWhen :: Bool -> Doc
breakWhen True   = line
breakWhen False  = space


isSimpleX :: Expr a -> Bool
isSimpleX xx
 = case xx of
        Var{}           -> True
        Lit{}           -> True
        App x1 x2       -> isSimpleX x1 && isAtomX x2
        Cast x1 _       -> isSimpleX x1
        _               -> False

isAtomX :: Expr a -> Bool
isAtomX xx
 = case xx of
        Var{}           -> True
        Lit{}           -> True
        _               -> False


parens' :: Doc -> Doc
parens' d = lparen <> nest 1 d <> rparen


-- | Wrap a `Doc` in parens if the predicate is true.
pprParen' :: Bool -> Doc -> Doc
pprParen' b c
 = if b then parens' c
        else c

