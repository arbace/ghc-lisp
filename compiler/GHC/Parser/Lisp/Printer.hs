{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The ghc-lisp printer: a parsed module (@HsModule GhcPs@) -> Lisp text
-- (SPEC.md). This is the hs2lisp direction; it covers every constructor of
-- the parsed syntax tree, so that the corpus round trip can check the
-- parser against it.
module GHC.Parser.Lisp.Printer
  ( lispModule
  , lispHeaderPragmas
  , PrintOpts(..)
  ) where

import GHC.Prelude

import GHC.Hs hiding (patNeedsParens)
import GHC.Parser.Lisp.Parens
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.PkgQual
import GHC.Types.InlinePragma
import Language.Haskell.Syntax.Decls.Overlap
import Language.Haskell.Syntax.Specificity
import Language.Haskell.Syntax.BooleanFormula
import Language.Haskell.Syntax.Text (HText, unpackHText)
import GHC.Unit.Module.Warnings
import GHC.Data.FastString
import GHC.Utils.Outputable

import Data.List.NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NE
import Data.Char (isAlpha, isUpper, isDigit)
import Data.Maybe (isJust)

data PrintOpts = PrintOpts { prParens :: ParenOpts }

-- | Header pragmas: @(:language ...)@, @(:options-ghc ...)@.
lispHeaderPragmas :: [String] -> [String] -> SDoc
lispHeaderPragmas exts opts = vcat $
  [ parens (text ":language" <+> hsep (map text exts)) | not (null exts) ] ++
  [ parens (text ":options-ghc" <+> hsep (map (doubleQuotes . text) opts)) | not (null opts) ]

lispModule :: PrintOpts -> HsModule GhcPs -> SDoc
lispModule o m = vcat (punctuate (text "") (header ++ imports ++ decls))
  where
    header = case hsmodName m of
      Nothing -> []
      Just (L _ mn) ->
        [ form (text "module" <+> ppr mn)
            (maybe [] (\w -> [warningTxt w]) (hsmodDeprecMessage (hsmodExt m)) ++
             maybe [] (\ies -> [ieList ies]) (hsmodExports m)) ]
    imports = [ vcat (map (importDecl . unLoc) (hsmodImports m)) | not (null (hsmodImports m)) ]
    decls = map (decl o TopCtx . unLoc) (hsmodDecls m)

-------------------------------------------------------------------------------
-- Layout helpers

-- | @(head arg ...)@, on one line if it fits, otherwise hanging.
form :: SDoc -> [SDoc] -> SDoc
form h [] = parens h
form h as = parens (hang h 2 (sep as))

-- | @(head fixed ...@ newline @body ...)@: bodies always go one per line.
formV :: SDoc -> [SDoc] -> SDoc
formV h [] = parens h
formV h body = parens (hang h 2 (vcat body))

vec :: [SDoc] -> SDoc
vec = brackets . fsep

todo :: String -> SDoc
todo s = parens (text ":todo" <+> doubleQuotes (text s))

-------------------------------------------------------------------------------
-- Names

-- | A name as a Lisp symbol.
rdr :: RdrName -> SDoc
rdr = \case
  Unqual occ -> occ' occ
  Qual mn occ -> ppr mn <> char '.' <> occ' occ
  Orig _ occ -> occ' occ
  r@(Exact _) -> exact (rdrNameOcc r)
  where
    occ' = ftext . occNameFS

-- | Names of built-in syntax: unit, lists, tuples, the function arrow.
exact :: OccName -> SDoc
exact occ = case occNameString occ of
  "()" -> text "()"
  "Unit" | isTcOcc occ -> text "()"
  "[]" -> text "[]"
  "List" | isTcOcc occ -> text "[]"
  ":" -> text ":"
  "->" -> parens (text ":name ->")
  "FUN" -> parens (text ":name ->")
  "Unit#" -> tupleCon ":utuple-con" 0
  "Solo#" -> tupleCon ":utuple-con" 1
  "MkSolo#" -> tupleCon ":utuple-con" 1
  "Solo" -> tupleCon ":tuple-con" 1
  "MkSolo" -> tupleCon ":tuple-con" 1
  '(':'#':r | all (`elem` "_| ") (takeWhile (/= '#') r), '|' `elem` r ->
    -- an unboxed sum data constructor: (:usum-con alt arity)
    let slots = splitBars (filter (/= ' ') (takeWhile (/= '#') r))
        alt = length (takeWhile (/= "_") slots) + 1
    in parens (text ":usum-con" <+> int alt <+> int (length slots))
  s | Just n <- sumTc s -> tupleCon ":usum-con" n
  s | Just n <- tupleTc s -> tupleCon ":tuple-con" n
    | Just n <- utupleTc s -> tupleCon ":utuple-con" n
    | '(':'#':r <- s, all (== ',') (takeWhile (/= '#') r) ->
        tupleCon ":utuple-con" (let c = length (takeWhile (== ',') r) in if c == 0 then 0 else c + 1)
    | '(':r <- s, all (== ',') (takeWhile (/= ')') r), not (null r) ->
        tupleCon ":tuple-con" (length (takeWhile (== ',') r) + 1)
    | otherwise -> ftext (occNameFS occ)
  where
    tupleCon k n = parens (text k <+> int n)
    splitBars str = case break (== '|') str of
      (a, []) -> [a]
      (a, _ : rest) -> a : splitBars rest
    sumTc s = case splitAt 3 s of
      ("Sum", ds) | not (null ds), last ds == '#', all isDigit (init ds), not (null (init ds)) -> Just (read (init ds))
      _ -> Nothing
    tupleTc s = case splitAt 5 s of
      ("Tuple", ds) | not (null ds), all isDigit ds -> Just (read ds)
      _ -> Nothing
    utupleTc s = case splitAt 5 s of
      ("Tuple", ds) | not (null ds), last ds == '#', all isDigit (init ds), not (null (init ds)) -> Just (read (init ds))
      _ -> Nothing

lname :: GenLocated l RdrName -> SDoc
lname = rdr . unLoc

isSymName :: RdrName -> Bool
isSymName (Exact _) = False
isSymName r = case occNameString (rdrNameOcc r) of
  c:_ -> not (isAlpha c || c == '_' || c == '(' || c == '[')
  _ -> False

-- | A constructor-like name (uppercase or starting with @:@).
isConName :: RdrName -> Bool
isConName r = case occNameString (rdrNameOcc r) of
  c:_ -> isUpper c || c == ':' || c == '(' || c == '['
  _ -> False

-- | An operator name in head position must be written @(:name op)@ when the
-- head would otherwise be read as an operator form.
headName :: RdrName -> SDoc
headName r
  | isSymName r = parens (text ":name" <+> rdr r)
  | otherwise = rdr r

-------------------------------------------------------------------------------
-- Literals

srcOr :: SourceText -> SDoc -> SDoc
srcOr (SourceText s) _ = ftext s
srcOr NoSourceText d = d

hsLit :: HsLit GhcPs -> SDoc
hsLit = \case
  HsChar st c -> srcOr st (pprHsChar c)
  HsCharPrim st c -> srcOr st (pprHsChar c <> char '#')
  HsString st s -> srcOr st (doubleQuotes (htext s))
  HsStringPrim st s -> srcOr st (pprHsBytes s)
  HsDouble _ fl -> fracLit fl
  HsNatural _ il -> intLit il
  HsInt _ il -> intLit il
  HsIntPrim st i -> srcOr st (integer i <> char '#')
  HsWordPrim st i -> srcOr st (integer i <> text "##")
  HsInt8Prim st i -> srcOr st (integer i)
  HsInt16Prim st i -> srcOr st (integer i)
  HsInt32Prim st i -> srcOr st (integer i)
  HsInt64Prim st i -> srcOr st (integer i)
  HsWord8Prim st i -> srcOr st (integer i)
  HsWord16Prim st i -> srcOr st (integer i)
  HsWord32Prim st i -> srcOr st (integer i)
  HsWord64Prim st i -> srcOr st (integer i)
  HsFloatPrim _ fl -> fracLit fl <> char '#'
  HsDoublePrim _ fl -> fracLit fl <> text "##"

intLit :: IntegralLit GhcPs -> SDoc
intLit il = srcOr (il_text il) (integer (il_value il))

fracLit :: FractionalLit GhcPs -> SDoc
fracLit fl = srcOr (fl_text fl) (ppr fl)

overLit :: HsOverLit GhcPs -> SDoc
overLit ol = case ol_val ol of
  HsIntegral il -> intLit il
  HsFractional fl -> fracLit fl
  HsIsString sl -> stringLit sl

stringLit :: StringLiteral GhcPs -> SDoc
stringLit sl = srcOr (sl_src sl) (doubleQuotes (htext (sl_fs sl)))

qualLit :: HsQualLit GhcPs -> SDoc
qualLit (QualLit _ mn v) = case v of
  HsQualString st s -> form (text ":qual-lit") [ppr mn, srcOr st (doubleQuotes (htext s))]

-------------------------------------------------------------------------------
-- Module header, imports, exports

ieList :: [LIE GhcPs] -> SDoc
ieList ies = vec (map (ie . unLoc) ies)

ie :: IE GhcPs -> SDoc
ie = \case
  IEVar w (L _ n) _ -> warned w (ieName n)
  IEThingAbs w (L _ n) _ -> warned w (ieName n)
  IEThingAll ext ns (L _ n) _ -> warned (ieta_warning ext) (namespaced ns (parens (ieName n <+> text "..")))
  IEThingWith (w, _) n wc subs _ ->
    let items = map (ieName . unLoc) subs
        items' = case wc of
          NoIEWildcard -> items
          IEWildcard i -> take i items ++ [text ".."] ++ drop i items
    in warned w (parens (hsep (ieName (unLoc n) : if null items' then [text ":_"] else items')))
  IEModuleContents (w, _) (L _ mn) -> warned w (parens (text "module" <+> ppr mn))
  IEWholeNamespace ext ns -> warned (iewn_warning ext) (namespaced ns (text ".."))
  IEGroup _ _ _ -> empty
  IEDoc _ _ -> empty
  IEDocNamed _ _ -> empty
  where
    ieName :: IEWrappedName GhcPs -> SDoc
    ieName = \case
      IEName _ n -> lname n
      IEDefault _ n -> parens (text "default" <+> lname n)
      IEPattern _ n -> parens (text "pattern" <+> lname n)
      IEType _ n -> parens (text "type" <+> lname n)
      IEData _ n -> parens (text "data" <+> lname n)
    warned Nothing d = d
    warned (Just w) d = form (warningHead (unLoc w)) (warningArgs (unLoc w) ++ [d])

namespaced :: NamespaceSpecifier GhcPs -> SDoc -> SDoc
namespaced ns d = case ns of
  NoNamespaceSpecifier _ -> d
  TypeNamespaceSpecifier _ -> parens (text "type" <+> d)
  DataNamespaceSpecifier _ -> parens (text "data" <+> d)

importDecl :: ImportDecl GhcPs -> SDoc
importDecl d = form (text "import") $
  [ text ":source" | ideclSource d == IsBoot ] ++
  [ text "safe" | ideclSafe d ] ++
  levelPre ++
  [ text "qualified" | QualifiedPre <- [ideclQualified d] ] ++
  pkg ++
  [ ppr (unLoc (ideclName d)) ] ++
  levelPost ++
  [ text "qualified" | QualifiedPost <- [ideclQualified d] ] ++
  maybe [] (\(L _ n) -> [text "as", ppr n]) (ideclAs d) ++
  case ideclImportList d of
    Nothing -> []
    Just (Exactly, ies) -> [ieList ies]
    Just (EverythingBut, ies) -> [text "hiding", ieList ies]
  where
    pkg = case ideclPkgQual d of
      RawPkgQual st fs -> [srcOr st (doubleQuotes (ftext fs))]
      NoRawPkgQual -> []
    lvl ImportDeclQuote = text "quote"
    lvl ImportDeclSplice = text "splice"
    levelPre = case ideclLevelSpec d of LevelStylePre l -> [lvl l]; _ -> []
    levelPost = case ideclLevelSpec d of LevelStylePost l -> [lvl l]; _ -> []

-------------------------------------------------------------------------------
-- Warnings

warningTxt :: LWarningTxt GhcPs -> SDoc
warningTxt (L _ w) = form (warningHead w) (warningArgs w)

warningHead :: WarningTxt GhcPs -> SDoc
warningHead = \case
  DeprecatedTxt{} -> text ":deprecated"
  WarningTxt{} -> text ":warning"

warningArgs :: WarningTxt GhcPs -> [SDoc]
warningArgs = \case
  DeprecatedTxt _ msgs -> [msgsDoc msgs]
  WarningTxt _ mcat msgs ->
    maybe [] (\(L _ (InWarningCategory (_, st) (L _ (WarningCategory c)))) ->
                [text "in", srcOr st (doubleQuotes (htext c))]) mcat ++
    [msgsDoc msgs]
  where
    msgsDoc [L _ m] = stringLit (hsDocString m)
    msgsDoc ms = vec [ stringLit (hsDocString m) | L _ m <- ms ]

-------------------------------------------------------------------------------
-- Declarations

data DeclCtx = TopCtx | ClassCtx | InstCtx | LocalCtx
  deriving Eq

decl :: PrintOpts -> DeclCtx -> HsDecl GhcPs -> SDoc
decl o ctx = \case
  TyClD _ d -> tyClDecl o ctx d
  InstD _ d -> instDecl o ctx d
  DerivD _ d -> derivDecl o d
  ValD _ b -> bind o b
  SigD _ s -> sig o ctx s
  KindSigD _ (StandaloneKindSig _ n t) ->
    form (text "type") [form (text "::") [lname n, sigType o t]]
  DefD _ d -> defaultDecl o d
  ForD _ d -> foreignDecl o d
  WarningD _ (Warnings _ ws) -> vcat (map (warnDecl . unLoc) ws)
  AnnD _ (HsAnnotation _ prov e) -> form (text ":ann") $ (case prov of
      ValueAnnProvenance n -> [lname n]
      TypeAnnProvenance n -> [text "type", lname n]
      ModuleAnnProvenance -> [text "module"]) ++ [expr o ETop e]
  RuleD _ (HsRules _ rs) -> formV (text ":rules") (map (ruleDecl o . unLoc) rs)
  SpliceD _ (SpliceDecl _ (L _ sp) deco) -> case deco of
    DollarSplice -> untypedSplice o sp
    BareSplice -> case sp of
      HsUntypedSpliceExpr _ e -> expr o ETop e
      _ -> untypedSplice o sp
  DocD _ _ -> empty
  RoleAnnotD _ (RoleAnnotDecl _ n roles) ->
    form (text "type role") (lname n : map (maybe (text "_") ppr . unLoc) roles)

warnDecl :: WarnDecl GhcPs -> SDoc
warnDecl (Warning _ ns names w) =
  form (warningHead w) (init args ++ namespace ++ map lname names ++ [last args])
  where
    args = warningArgs w
    namespace = case ns of
      NoNamespaceSpecifier _ -> []
      TypeNamespaceSpecifier _ -> [text "type"]
      DataNamespaceSpecifier _ -> [text "data"]

ruleDecl :: PrintOpts -> RuleDecl GhcPs -> SDoc
ruleDecl o (HsRule _ (L _ name) act bndrs lhs rhs) =
  form (doubleQuotes (htext name)) $
    activation act ++ ruleBndrs o bndrs ++
    [form (text "=") [expr o ETop lhs, expr o ETop rhs]]

ruleBndrs :: PrintOpts -> RuleBndrs GhcPs -> [SDoc]
ruleBndrs o (RuleBndrs _ mtvs tms) =
  maybe [] (\tvs -> [form (text "forall") (map (tyVarBndr o (const id) . unLoc) tvs)]) mtvs ++
  [ form (text "forall") (map (ruleBndr . unLoc) tms) | isJust mtvs || not (null tms) ]
  where
    ruleBndr = \case
      RuleBndr _ n -> lname n
      RuleBndrSig _ n (HsPS _ t) -> form (text "::") [lname n, typ o TTop t]

activation :: ActivationGhc -> [SDoc]
activation = \case
  AlwaysActive -> []
  ActiveBefore n -> [brackets (char '~' <+> int n)]
  ActiveAfter n -> [brackets (int n)]
  NeverActive -> [brackets (char '~')]
  _ -> []

defaultDecl :: PrintOpts -> DefaultDecl GhcPs -> SDoc
defaultDecl o (DefaultDecl _ mods mcls tys) = modified o mods $ case mcls of
  Nothing -> form (text "default") (map (typ o TArg) tys)
  Just c -> form (text "default") [lname c, vec (map (typ o TParen) tys)]

foreignDecl :: PrintOpts -> ForeignDecl GhcPs -> SDoc
foreignDecl o = \case
  ForeignImport _ mods n t (CImport (L _ src) (L _ cconv) (L _ safety) _ _) ->
    modified o mods $ form (text "foreign import") $
      [ccallConv cconv, safetyDoc safety] ++ entity src ++
      [form (text "::") [lname n, sigType o t]]
  ForeignExport _ mods n t (CExport (L _ src) (L _ (CExportStatic _ cconv))) ->
    modified o mods $ form (text "foreign export") $
      [ccallConv cconv] ++ entity src ++ [form (text "::") [lname n, sigType o t]]
  where
    entity = \case
      SourceText s -> [ftext s]
      NoSourceText -> []
    safetyDoc = \case
      PlaySafe -> text "safe"
      PlayInterruptible -> text "interruptible"
      PlayRisky -> text "unsafe"
    ccallConv = \case
      CCallConv -> text "ccall"
      CApiConv -> text "capi"
      StdCallConv -> text "stdcall"
      PrimCallConv -> text "prim"
      JavaScriptCallConv -> text "javascript"

modified :: PrintOpts -> [LHsModifier GhcPs] -> SDoc -> SDoc
modified _ [] d = d
modified o mods d = form (text ":mod") (map (modifier o) mods ++ [d])

modifier :: PrintOpts -> LHsModifier GhcPs -> SDoc
modifier o (L _ (HsModifier _ t)) = typ o TArg t

-- | The head of a type-level declaration: @T@, @(T a b)@, @(:infix a + b)@.
declHead :: PrintOpts -> LIdP GhcPs -> LHsQTyVars GhcPs -> LexicalFixity -> SDoc
declHead o n (HsQTvs _ tvs) fix = case (fix, tvs) of
  (_, []) -> lname n
  (Infix, l : r : rest) ->
    let inf = form (text ":infix") [bndr l, lname n, bndr r]
    in if null rest then inf else form inf (map bndr rest)
  _ -> form (headName (unLoc n)) (map bndr tvs)
  where
    bndr = tyVarBndr o visFlag . unLoc

visFlag :: HsBndrVis GhcPs -> SDoc -> SDoc
visFlag = \case
  HsBndrInvisible _ -> (char '@' <>)
  _ -> id

tyVarBndr :: PrintOpts -> (flag -> SDoc -> SDoc) -> HsTyVarBndr flag GhcPs -> SDoc
tyVarBndr o flagDoc (HsTvb _ flag var kind) = flagDoc flag $ case kind of
  HsBndrNoKind _ -> v
  HsBndrKind _ k -> form (text "::") [v, typ o TTop k]
  where
    v = case var of
      HsBndrVar _ n -> lname n
      HsBndrWildCard _ -> text "_"

specFlag :: Specificity -> SDoc -> SDoc
specFlag = \case
  InferredSpec -> \d -> parens (text ":inferred" <+> d)
  SpecifiedSpec -> id

tyClDecl :: PrintOpts -> DeclCtx -> TyClDecl GhcPs -> SDoc
tyClDecl o ctx = \case
  FamDecl _ fd -> familyDecl o ctx fd
  SynDecl _ n tvs fix rhs ->
    form (text "type") [declHead o n tvs fix, typ o TTop rhs]
  DataDecl _ mods n tvs fix defn ->
    modified o mods $ dataDefn o (dataKeyword defn) (declHead o n tvs fix) defn
  ClassDecl _ mods mctx n tvs fix fds decls ->
    modified o mods $ formV (text "class" <+> withCtx o mctx (declHead o n tvs fix)
                              <+> fundeps fds)
      (map (decl o ClassCtx . unLoc) decls)
  where
    fundeps [] = empty
    fundeps fds = form (text "|") [ parens (hsep (map lname l) <+> text "->" <+> hsep (map lname r))
                                  | L _ (FunDep _ l r) <- fds ]

dataKeyword :: HsDataDefn GhcPs -> SDoc
dataKeyword defn = case dd_cons defn of
  NewTypeCon _ -> text "newtype"
  DataTypeCons True _ -> text "type data"
  DataTypeCons False _ -> text "data"

withCtx :: PrintOpts -> Maybe (LHsContext GhcPs) -> SDoc -> SDoc
withCtx _ Nothing d = d
withCtx o (Just (L _ (HsContext _ cs))) d = form (text "=>") (map (typ o TCtxElem) cs ++ [d])

dataDefn :: PrintOpts -> SDoc -> SDoc -> HsDataDefn GhcPs -> SDoc
dataDefn o kw hd (HsDataDefn _ mctx mctype mkind cons derivs) =
  formV (kw <+> ctypeDoc <+> withCtx o mctx hd') (consDocs ++ map (derivClause o . unLoc) derivs)
  where
    hd' = case mkind of
      Nothing -> hd
      Just k -> form (text "::") [hd, typ o TTop k]
    ctypeDoc = case mctype of
      Nothing -> empty
      Just (L _ (CType ext mh name)) ->
        form (text ":ctype") (maybe [] (\(Header hs h) -> [srcOr hs (doubleQuotes (htext h))]) mh ++
                              [srcOr (cTypeOtherText ext) (doubleQuotes (htext name))])
    conList = case cons of
      NewTypeCon c -> [c]
      DataTypeCons _ cs -> cs
    consDocs = case conList of
      cs@(L _ ConDeclGADT{} : _) -> [formV (text "where") (map (conDecl o . unLoc) cs)]
      cs -> map (conDecl o . unLoc) cs

derivClause :: PrintOpts -> HsDerivingClause GhcPs -> SDoc
derivClause o (HsDerivingClause _ mstrat (L _ tys)) = form (text "deriving") $
  pre ++ [clauseTys] ++ post
  where
    clauseTys = case tys of
      DctSingle _ t -> sigType o t
      DctMulti _ ts -> vec (map (sigType o) ts)
    (pre, post) = case fmap unLoc mstrat of
      Nothing -> ([], [])
      Just (StockStrategy _) -> ([text "stock"], [])
      Just (AnyclassStrategy _) -> ([text "anyclass"], [])
      Just (NewtypeStrategy _) -> ([text "newtype"], [])
      Just (ViaStrategy (XViaStrategyPs _ t)) -> ([], [text "via", sigType o t])

conDecl :: PrintOpts -> ConDecl GhcPs -> SDoc
conDecl o = \case
  ConDeclH98 _ mods n hasForall exTvs mctx args _ ->
    modified o mods $
      let core = case args of
            PrefixCon _ [] -> lname n
            PrefixCon _ fs -> form (headName (unLoc n)) (map (conField o TArg) fs)
            InfixCon _ l r -> form (text ":infix") [conField o TOperand l, lname n, conField o TOperand r]
            RecCon _ (L _ []) -> form (text ":rec") [lname n]
            RecCon _ (L _ flds) -> form (headName (unLoc n)) (map (recField o . unLoc) flds)
          withC = withCtx o mctx core
      in if hasForall
           then form (text "forall") (map (tyVarBndr o specFlag . unLoc) exTvs ++ [withC])
           else withC
  ConDeclGADT _ mods names (L _ outer) inner mctx args res _ ->
    modified o mods $ form (text "::") (map lname (toList names) ++ [ty])
    where
      argsDocs = case args of
        PrefixConGADT _ fs -> map (\f -> (Just (cdf_multiplicity f), conField o TArrowArg f)) fs
        RecConGADT _ (L _ flds) ->
          [(Nothing, form (text ":record") (map (recField o . unLoc) flds))]
      body = arrows o argsDocs (typ o TTop res)
      withC = withCtx o mctx body
      withInner = foldr (\(L _ tele) d -> case tele of
                          HsGadtForAll _ t -> forallTele o t d
                          HsGadtPar _ -> d) withC inner
      ty = case outer of
        HsOuterImplicit _ -> withInner
        HsOuterExplicit _ bndrs -> form (text "forall") (map (tyVarBndr o specFlag . unLoc) bndrs ++ [withInner])

-- | An arrow chain from (arrow, argument) pairs and a result.
arrows :: PrintOpts -> [(Maybe (HsModifiedFunArr GhcPs), SDoc)] -> SDoc -> SDoc
arrows _ [] res = res
arrows o args res
  | all (maybe True isPlainArr . fst) args = form (text "->") (map snd args ++ [res])
  | otherwise = foldr one res args
  where
    one (Nothing, a) r = form (text "->") [a, r]
    one (Just arr, a) r = form (arrowHead arr) [arrowArg o arr a, r]

isPlainArr :: HsModifiedFunArr GhcPs -> Bool
isPlainArr (HsModifiedFunArr _ [] (HsStandardArr _)) = True
isPlainArr _ = False

arrowHead :: HsModifiedFunArr GhcPs -> SDoc
arrowHead (HsModifiedFunArr _ _ arr) = case arr of
  HsStandardArr _ -> text "->"
  HsLinearArr _ -> text "->."

arrowArg :: PrintOpts -> HsModifiedFunArr GhcPs -> SDoc -> SDoc
arrowArg o (HsModifiedFunArr _ mods _) a = case mods of
  [] -> a
  _ -> form (text ":mod") (map (modifier o) mods ++ [a])

conField :: PrintOpts -> TPos -> HsConDeclField GhcPs -> SDoc
conField o pos (CDF _ unpack bang _ t _) = unpackDoc (bangDoc tyDoc)
  where
    tyDoc = case t of
      L _ (HsParTy _ k@(L _ HsKindSig{})) | pos == TArg -> form (text ":paren") [typ o TParen k]
      _ -> typ o pos' t
    pos' = if bang /= NoSrcStrict || unpack /= NoSrcUnpack then TArg else pos
    bangDoc d = case bang of
      SrcStrict -> form (text "!") [d]
      SrcLazy -> form (text "~") [d]
      NoSrcStrict -> d
    unpackDoc d = case unpack of
      SrcUnpack -> form (text ":unpack") [d]
      SrcNoUnpack -> form (text ":nounpack") [d]
      NoSrcUnpack -> d

recField :: PrintOpts -> HsConDeclRecField GhcPs -> SDoc
recField o (HsConDeclRecField _ names cdf) =
  form (text "::") (map (fieldOcc . unLoc) names ++ [ty])
  where
    ty = case cdf_multiplicity cdf of
      HsModifiedFunArr _ [] _ -> conField o TTop cdf
      HsModifiedFunArr _ mods _ -> form (text ":mod") (map (modifier o) mods ++ [conField o TTop cdf])

fieldOcc :: FieldOcc GhcPs -> SDoc
fieldOcc (FieldOcc _ n) = lname n

familyDecl :: PrintOpts -> DeclCtx -> FamilyDecl GhcPs -> SDoc
familyDecl o ctx (FamilyDecl _ info top n tvs fix (L _ res) minj) =
  formV (kw <+> hd' <+> resDoc <+> injDoc) eqns
  where
    kw = case info of
      DataFamily -> text (if isTopLevel top || ctx /= ClassCtx then "data family" else "data")
      _ -> text (if ctx == ClassCtx then "type" else "type family")
    hd = declHead o n tvs fix
    (hd', resDoc) = case res of
      NoSig _ -> (hd, empty)
      KindSig _ k -> (form (text "::") [hd, typ o TTop k], empty)
      TyVarSig _ (L _ b) -> (hd, form (text "=") [tyVarBndr o (const id) b])
    injDoc = case minj of
      Nothing -> empty
      Just (L _ (InjectivityAnn _ l rs)) -> form (text "|") [lname l, text "->", hsep (map lname rs)]
    eqns = case info of
      ClosedTypeFamily Nothing -> [form (text "where") [text ".."]]
      ClosedTypeFamily (Just es) -> [formV (text "where") (map (famEqn o (typ o TTop) . unLoc) es)]
      _ -> []

famEqn :: PrintOpts -> (rhs -> SDoc) -> FamEqn GhcPs rhs -> SDoc
famEqn o rhsDoc (FamEqn _ n bndrs pats fix rhs) =
  outerForall o (const id) bndrs (form (text "=") [famLhs o n pats fix, rhsDoc rhs])

famLhs :: PrintOpts -> LIdP GhcPs -> HsFamEqnPats GhcPs -> LexicalFixity -> SDoc
famLhs o n pats fix = case (fix, valArgs) of
  (Infix, [l, r]) -> form (text ":infix") [l, lname n, r]
  _ | null pats -> lname n
    | otherwise -> form (headName (unLoc n)) (map typeArg pats)
  where
    valArgs = [ typ o TOperand t | HsValArg _ t <- pats ]
    typeArg = \case
      HsValArg _ t -> typ o TArg t
      HsTypeArg _ k -> char '@' <> typ o TArg k
      HsArgPar _ -> empty

outerForall :: PrintOpts -> (flag -> SDoc -> SDoc) -> HsOuterTyVarBndrs flag GhcPs -> SDoc -> SDoc
outerForall o flagDoc outer d = case outer of
  HsOuterImplicit _ -> d
  HsOuterExplicit _ bs -> form (text "forall") (map (tyVarBndr o flagDoc . unLoc) bs ++ [d])

instDecl :: PrintOpts -> DeclCtx -> InstDecl GhcPs -> SDoc
instDecl o ctx = \case
  ClsInstD _ (ClsInstDecl (mwarn, _) mods ty decls moverlap) ->
    modified o mods $ formV (text "instance" <+> maybe empty warningTxt mwarn
                              <+> overlap moverlap <+> sigType o ty)
      (map (decl o InstCtx . unLoc) decls)
  DataFamInstD _ (DataFamInstDecl (FamEqn _ n bndrs pats fix defn)) ->
    let kw = dataKeyword defn
        kw' = if ctx == InstCtx then kw else kw <+> text "instance"
    in dataDefn o kw' (outerForall o (const id) bndrs (famLhs o n pats fix)) defn
  TyFamInstD _ (TyFamInstDecl _ eqn) ->
    form (text (if ctx == InstCtx then "type" else "type instance")) [famEqn o (typ o TTop) eqn]

overlap :: Maybe (LocatedA (OverlapMode GhcPs)) -> SDoc
overlap = \case
  Nothing -> empty
  Just (L _ m) -> text $ case m of
    NoOverlap _ -> ":no-overlap"
    Overlappable _ -> ":overlappable"
    Overlapping _ -> ":overlapping"
    Overlaps _ -> ":overlaps"
    Incoherent _ -> ":incoherent"
    NonCanonical _ -> ":noncanonical"

derivDecl :: PrintOpts -> DerivDecl GhcPs -> SDoc
derivDecl o (DerivDecl (mwarn, _) (HsWC _ ty) mstrat moverlap) =
  form (text "deriving") $ strat ++ [text "instance"] ++ [maybe empty warningTxt mwarn | isJust mwarn]
    ++ [overlap moverlap | isJust moverlap] ++ [sigType o ty]
  where
    strat = case fmap unLoc mstrat of
      Nothing -> []
      Just (StockStrategy _) -> [text "stock"]
      Just (AnyclassStrategy _) -> [text "anyclass"]
      Just (NewtypeStrategy _) -> [text "newtype"]
      Just (ViaStrategy (XViaStrategyPs _ t)) -> [text "via", sigType o t]

-------------------------------------------------------------------------------
-- Bindings and signatures

bind :: PrintOpts -> HsBind GhcPs -> SDoc
bind o = \case
  FunBind _ n (MG _ (L _ ms)) -> vcat (map (funEquation o n . unLoc) ms)
  PatBind _ p mods grhss ->
    form (text "=") (modified o mods (pat o PTop p) : grhssDocs o grhss)
  VarBind{} -> todo "VarBind"
  PatSynBind _ psb -> patSynBind o psb

funEquation :: PrintOpts -> LIdP GhcPs -> Match GhcPs (LHsExpr GhcPs) -> SDoc
funEquation o n (Match _ ctxt (L _ pats) grhss) =
  form (text "=") (lhs : grhssDocs o grhss)
  where
    (fixity, strict) = case ctxt of
      FunRhs { mc_fixity = f, mc_strictness = s } -> (f, s)
      _ -> (Prefix, NoSrcStrict)
    strictWrap d = case strict of
      SrcStrict -> form (text "!") [d]
      _ -> d
    lhs = strictWrap $ case (fixity, pats) of
      (Infix, l : r : rest)
        | isConChain l || isConChain r ->
            let inf = form (text ":infix") (chainItems l ++ [lname n] ++ chainItems r)
            in if null rest then inf else form inf (map (pat o PArg) rest)
      (Infix, l : r : rest) ->
        let inf = infixLhs (pat o POperand l) (pat o POperand r)
        in if null rest then inf else form inf (map (pat o PArg) rest)
      (_, []) -> lname n
      (_, ps) -> form (headName (unLoc n)) (map (pat o PArg) ps)
    infixLhs l r
      | isSymName (unLoc n) = form (lname n) [l, r]
      | otherwise = form (text ":infix") [l, lname n, r]
    chainItems p@(L _ (ConPat _ _ InfixCon{})) = conChainItems o p
    chainItems p = [pat o POperand p]

-- | An unparenthesized infix constructor pattern, as in @f :+ g <*> a :+ b@.
isConChain :: LPat GhcPs -> Bool
isConChain (L _ (ConPat _ _ InfixCon{})) = True
isConChain _ = False

grhssDocs :: PrintOpts -> GRHSs GhcPs (LHsExpr GhcPs) -> [SDoc]
grhssDocs o (GRHSs _ grhss binds) = rhs ++ whereDoc o binds
  where
    rhs = case grhss of
      L _ (GRHS _ [] e) :| [] -> [expr o ETop e]
      _ -> map (grhs o (expr o ETop) . unLoc) (toList grhss)

grhs :: PrintOpts -> (body -> SDoc) -> GRHS GhcPs body -> SDoc
grhs o bodyDoc (GRHS _ guards body) = form (text "|") (map (stmt o . unLoc) guards ++ [bodyDoc body])

whereDoc :: PrintOpts -> HsLocalBinds GhcPs -> [SDoc]
whereDoc o binds = case binds of
  EmptyLocalBinds _ -> []
  _ -> [formV (text "where") (localBinds o binds)]

localBinds :: PrintOpts -> HsLocalBinds GhcPs -> [SDoc]
localBinds o = \case
  HsValBinds _ (ValBinds _ vbs) -> map valBind vbs
  HsIPBinds _ (IPBinds _ ips) ->
    [ form (text "=") [char '?' <> htext n, expr o ETop e]
    | L _ (IPBind _ (L _ (HsIPName n)) e) <- ips ]
  EmptyLocalBinds _ -> []
  where
    valBind = \case
      VbBind (L _ b) -> bind o b
      VbSig (L _ s) -> sig o LocalCtx s

patSynBind :: PrintOpts -> PatSynBind GhcPs GhcPs -> SDoc
patSynBind o (PSB _ n details def dir) = form (text "pattern") $
  [lhs] ++ case dir of
    Unidirectional -> [text "<-", pat o PTop def]
    ImplicitBidirectional -> [text "=", pat o PTop def]
    ExplicitBidirectional (MG _ (L _ ms)) ->
      [text "<-", pat o PTop def, formV (text "where") (map (funEquation o n . unLoc) ms)]
  where
    lhs = case details of
      PrefixCon _ [] -> lname n
      PrefixCon _ args -> form (headName (unLoc n)) (map lname args)
      InfixCon _ l r -> form (text ":infix") [lname l, lname n, lname r]
      RecCon _ flds -> form (text ":rec") (lname n : map (\f -> lname (recordPatSynPatVar f)) flds)

sig :: PrintOpts -> DeclCtx -> Sig GhcPs -> SDoc
sig o ctx = \case
  TypeSig _ mods names (HsWC _ t) ->
    modified o mods $ form (text "::") (map lname names ++ [sigType o t])
  PatSynSig _ names t -> form (text "pattern") [form (text "::") (map lname names ++ [sigType o t])]
  ClassOpSig _ isDefault names t ->
    let s = form (text "::") (map lname names ++ [sigType o t])
    in if isDefault then form (text "default") [s] else s
  FixSig (_, src) (FixitySig _ ns names (Fixity prec dir)) ->
    form (fixityKw dir) ([ srcOr src (int prec) | isSourceText src ] ++ namespace ns ++ map lname names)
  InlineSig _ n prag
    | Opaque <- inl_inline prag -> form (text ":opaque") [lname n]
    | otherwise -> form (inlineHead prag) (inlineOpts prag ++ [lname n])
  SpecSig _ n tys prag ->
    form (text ":specialise") (specOpts prag ++ [lname n] ++ map (sigType o) tys)
  SpecSigE _ bndrs e prag ->
    form (text ":specialise") (specOpts prag ++ ruleBndrs o bndrs ++ [expr o ETop e])
  SpecInstSig _ t -> form (text ":specialise") [text "instance", sigType o t]
  MinimalSig _ (L _ bf) -> form (text ":minimal") [boolFormula bf]
  SCCFunSig _ n mlbl ->
    form (text ":scc") (lname n : maybe [] (\(L _ sl) -> [stringLit sl]) mlbl)
  CompleteMatchSig _ names mty ->
    form (text ":complete") (map lname names ++ maybe [] (\t -> [text "::", lname t]) mty)
  where
    _ = ctx
    isSourceText = \case
      SourceText _ -> True
      NoSourceText -> False
    fixityKw = \case
      InfixL -> text "infixl"
      InfixR -> text "infixr"
      InfixN -> text "infix"
    namespace = \case
      NoNamespaceSpecifier _ -> []
      TypeNamespaceSpecifier _ -> [text "type"]
      DataNamespaceSpecifier _ -> [text "data"]
    inlineHead prag = text $ case inl_inline prag of
      Inline -> ":inline"
      Inlinable -> ":inlinable"
      NoInline -> ":noinline"
      Opaque -> ":opaque"
      NoUserInlinePrag -> ":inline"
    inlineOpts prag = activation (inl_act prag) ++
      [ text ":conlike" | ConLike <- [inl_rule prag] ]
    specOpts prag = (case inl_inline prag of
        Inline -> [text ":inline"]
        NoInline -> [text ":noinline"]
        _ -> []) ++ activation (inl_act prag)
    boolFormula = \case
      Var _ n -> lname n
      And _ bfs -> form (text ":and") (map (boolFormula . unLoc) bfs)
      Or _ bfs -> form (text ":or") (map (boolFormula . unLoc) bfs)
      Parens _ (L _ bf) -> form (text ":paren") [boolFormula bf]

-------------------------------------------------------------------------------
-- Types

sigType :: PrintOpts -> LHsSigType GhcPs -> SDoc
sigType o (L _ (HsSig _ outer body)) = case outer of
  HsOuterImplicit _ -> typ o TTop body
  HsOuterExplicit _ bs -> form (text "forall") (map (tyVarBndr o specFlag . unLoc) bs ++ [typ o TTop body])

forallTele :: PrintOpts -> HsForAllTelescope GhcPs -> SDoc -> SDoc
forallTele o tele body = case tele of
  HsForAllInvis _ bs -> form (text "forall") (map (tyVarBndr o specFlag . unLoc) bs ++ [body])
  HsForAllVis _ bs -> form (text "forall") (map (tyVarBndr o (const id) . unLoc) bs ++ [text "->", body])

-- | A type at a position: implicit parentheses per SPEC I5.
typ :: PrintOpts -> TPos -> LHsType GhcPs -> SDoc
typ o pos (L _ t) = case t of
  HsParTy _ (L _ inner)
    | typeNeedsParens pos inner -> typeForm o inner
    | otherwise -> form (text ":paren") [typ o TParen (L noSrcSpanA inner)]
  _ -> typeForm o t

typeForm :: PrintOpts -> HsType GhcPs -> SDoc
typeForm o = \case
  HsForAllTy _ tele body -> forallTele o tele (typ o TTop body)
  HsQualTy _ (L _ (HsContext _ cs)) body ->
    form (text "=>") (map (typ o TCtxElem) cs ++ [typ o TTop body])
  HsTyVar _ prom n -> promoted prom (lname n)
  t@HsAppTy{} -> typeApp o t
  t@HsAppKindTy{} -> typeApp o t
  t@(HsFunTy _ arr _ _) ->
    let (args, res) = funChain t
    in if all isPlainArr (map fst args)
         then form (text "->") (map (typ o TArrowArg . snd) args ++ [typ o TTop res])
         else case t of
           HsFunTy _ _ a r -> form (arrowHead arr) [arrowArg o arr (typ o TArrowArg a), typ o TTop r]
           _ -> empty
  HsListTy _ t -> brackets (typ o TTop t)
  HsTupleTy _ sort ts -> case (sort, ts) of
    (HsBoxedOrConstraintTuple, []) -> text "()"
    (HsBoxedOrConstraintTuple, _) -> form (text ":tuple") (map (typ o TParen) ts)
    (HsUnboxedTuple, _) -> form (text ":utuple") (map (typ o TParen) ts)
  HsSumTy _ ts -> form (text ":usum") (map (typ o TParen) ts)
  t@HsOpTy{} -> opTyChain o t
  HsParTy _ t -> form (text ":paren") [typ o TParen t]
  HsIParamTy _ (L _ (HsIPName n)) t -> form (text "::") [char '?' <> htext n, typ o TTop t]
  HsStarTy _ -> text "*"
  HsKindSig _ t k -> form (text "::") [typ o TSigSubj t, typ o TTop k]
  HsSpliceTy _ sp -> untypedSplice o sp
  HsDocTy _ t _ -> typ o TTop t
  HsExplicitListTy _ prom ts -> case (prom, ts) of
    (IsPromoted, L _ t1 : _) | startsWithTick t1 -> text "'[" <+> fsep (map (typ o TTop) ts) <> char ']'
    _ -> promoted prom (vec (map (typ o TTop) ts))
  HsExplicitTupleTy _ prom ts -> promoted prom (form (text ":tuple") (map (typ o TTop) ts))
  HsTyLit _ l -> hsLit l
  HsWildCardTy _ -> text "_"
  XHsType _ -> todo "XHsType"
  where
    startsWithTick = \case
      HsTyVar _ IsPromoted _ -> True
      HsExplicitListTy _ IsPromoted _ -> True
      HsExplicitTupleTy _ IsPromoted _ -> True
      _ -> False
    promoted IsPromoted d = char '\'' <> d
    promoted NotPromoted d = d

funChain :: HsType GhcPs -> ([(HsModifiedFunArr GhcPs, LHsType GhcPs)], LHsType GhcPs)
funChain = \case
  HsFunTy _ arr a r@(L _ rt) -> case rt of
    HsFunTy{} -> let (as, res) = funChain rt in ((arr, a) : as, res)
    _ -> ([(arr, a)], r)
  t -> ([], L noSrcSpanA t)

typeApp :: PrintOpts -> HsType GhcPs -> SDoc
typeApp o t = form hd (reverse args)
  where
    (hd, args) = go t
    go = \case
      HsAppTy _ (L _ f) a -> let (h, as) = go' f in (h, typ o TArg a : as)
      HsAppKindTy _ (L _ f) k -> let (h, as) = go' f in (h, (char '@' <> typ o TArg k) : as)
      other -> (typeForm o other, [])
    go' f = case f of
      HsAppTy{} -> go f
      HsAppKindTy{} -> go f
      HsTyVar _ NotPromoted (L _ n) | isSymName n -> (parens (text ":name" <+> rdr n), [])
      _ -> (typ o TFun (L noSrcSpanA f), [])

opTyChain :: PrintOpts -> HsType GhcPs -> SDoc
opTyChain o t
  | allSame, Just op1 <- headOp, symOp op1 = form (opDoc op1) (map (typ o TOperand) operands)
  | otherwise = form (text ":infix") (interleave (map (typ o TOperand) operands) (map opDoc ops))
  where
    (operands, ops) = flatten t
    flatten = \case
      HsOpTy _ l op (L _ r@HsOpTy{}) -> let (os, ps) = flatten r in (l : os, op : ps)
      HsOpTy _ l op r -> ([l, r], [op])
      _ -> ([], [])
    headOp = case ops of (op:_) -> Just op; [] -> Nothing
    allSame = case ops of
      (L _ (HsTyVar _ p1 (L _ n1)) : rest) ->
        all (\case L _ (HsTyVar _ p (L _ n)) -> n == n1 && p == p1; _ -> False) rest
      _ -> False
    symOp (L _ (HsTyVar _ _ (L _ n))) = isSymName n
    symOp _ = False
    opDoc (L _ (HsTyVar _ prom (L _ n))) = (if prom == IsPromoted then char '\'' else empty) <> rdr n
    opDoc (L _ op) = typeForm o op

interleave :: [a] -> [a] -> [a]
interleave (x:xs) (y:ys) = x : y : interleave xs ys
interleave xs [] = xs
interleave [] ys = ys

-------------------------------------------------------------------------------
-- Expressions

-- | An expression at a position: implicit parentheses per SPEC I5.
expr :: PrintOpts -> EPos -> LHsExpr GhcPs -> SDoc
expr o pos (L _ e) = case e of
  HsPar _ (L _ inner)
    | exprNeedsParens (prParens o) pos inner -> exprForm o inner
    | otherwise -> form (text ":paren") [expr o EParen (L noSrcSpanA inner)]
  _ -> exprForm o e

exprForm :: PrintOpts -> HsExpr GhcPs -> SDoc
exprForm o = \case
  HsVar _ n -> lname n
  HsOverLabel st l -> srcOr st (char '#' <> htext l)
  HsIPVar _ (HsIPName n) -> char '?' <> htext n
  HsOverLit _ ol -> overLit ol
  HsLit _ l -> hsLit l
  HsQualLit _ ql -> qualLit ql
  HsLam _ variant (MG _ (L _ ms)) -> case variant of
    LamSingle -> case ms of
      [L _ (Match _ _ (L _ ps) (GRHSs _ (L _ (GRHS _ [] body) :| []) _))] ->
        form (text "\\") (map (pat o PArg) ps ++ [expr o ETop body])
      _ -> todo "HsLam"
    LamCase -> formV (text "\\case") (map (alt o (expr o ETop) . unLoc) ms)
    LamCases -> formV (text "\\cases") (map (alt o (expr o ETop) . unLoc) ms)
  e@HsApp{} -> app o e
  e@HsAppType{} -> app o e
  e@OpApp{} -> opChain o e
  NegApp _ e _ -> form (text "-") [expr o ENegArg e]
  HsPar _ e -> form (text ":paren") [expr o EParen e]
  SectionL _ e op -> form (text ":section-l") [expr o ESectionL e, opName op]
  SectionR _ op e -> form (text ":section-r") [opName op, expr o ESectionR e]
  ExplicitTuple _ args boxity ->
    form (text (if isBoxed boxity then ":tuple" else ":utuple")) (map tupArg args)
  ExplicitSum _ tag width e ->
    form (text ":usum") (replicate (tag - 1) (text ":_") ++ [expr o ETop e] ++ replicate (width - tag) (text ":_"))
  HsCase _ scrut (MG _ (L _ ms)) ->
    formV (text "case" <+> expr o ETop scrut) (map (alt o (expr o ETop) . unLoc) ms)
  HsIf _ c t e -> form (text "if") [expr o ETop c, expr o ETop t, expr o ETop e]
  HsMultiIf _ grhss -> formV (text "if") (map (grhs o (expr o ETop) . unLoc) (toList grhss))
  HsLet _ binds body -> formV (text "let") (localBinds o binds ++ [expr o ETop body])
  HsDo _ flav (L _ stmts) -> doExpr o flav stmts
  ExplicitList _ es -> vec (map (expr o ETop) es)
  RecordCon _ (L _ con) flds -> form (text ":rec") (rdr con : recFields o (expr o ETop) flds)
  RecordUpd _ e flds -> form (text ":update") (expr o EAtom e : recUpdFields flds)
  HsGetField _ e f -> getField [f] e
  HsProjection _ flds -> form (text ":proj") (map dotField (toList flds))
  ExprWithTySig _ e (HsWC _ t) -> form (text "::") [expr o ESigSubj e, sigType o t]
  ArithSeq _ _ info -> case info of
    From a -> vec [expr o ETop a, text ".."]
    FromThen a b -> vec [expr o ETop a, expr o ETop b, text ".."]
    FromTo a c -> vec [expr o ETop a, text "..", expr o ETop c]
    FromThenTo a b c -> vec [expr o ETop a, expr o ETop b, text "..", expr o ETop c]
  HsTypedBracket _ e -> form (text ":typed-quote") [expr o ETop e]
  HsUntypedBracket _ q -> case q of
    ExpBr _ e -> form (text ":quote") [expr o ETop e]
    PatBr _ p -> form (text ":quote-pat") [pat o PTop p]
    DecBrL _ ds -> formV (text ":quote-decls") (map (decl o TopCtx . unLoc) ds)
    DecBrG{} -> todo "DecBrG"
    TypBr _ t -> form (text ":quote-type") [typ o TTop t]
    VarBr _ isValue n -> (if isValue then char '\'' else text "''") <> lname n
  HsTypedSplice _ (HsTypedSpliceExpr _ e) -> form (text ":typed-splice") [expr o EAtom e]
  HsUntypedSplice _ sp -> untypedSplice o sp
  HsProc _ p (L _ (HsCmdTop _ (L _ c))) -> form (text "proc") [pat o PArg p, cmd o c]
  HsStatic _ e -> form (text "static") [expr o EAtom e]
  HsPragE _ (HsPragSCC _ sl) e -> form (text ":scc") [sccLabel sl, expr o ETop e]
  HsEmbTy _ (HsWC _ t) -> form (text "type") [typ o TTop t]
  HsStar _ -> text "*"
  HsHole k -> case k of
    HoleVar n -> lname n
    HoleError -> todo "HoleError"
  HsForAll _ tele e -> forallTele o tele (expr o ETop e)
  HsQual _ (L _ (HsContext _ cs)) e -> form (text "=>") (map (expr o ETop) cs ++ [expr o ETop e])
  HsFunArr _ arr a b -> form (arrowHead' arr) [expr o ETop a, expr o ETop b]
  where
    tupArg = \case
      Present _ e -> expr o ETop e
      Missing _ -> text ":_"
    opName (L _ (HsVar _ n)) = lname n
    opName (L _ (HsHole (HoleVar n))) = lname n
    opName e = expr o ETop e
    getField acc (L _ (HsGetField _ e f)) = getField (f : acc) e
    getField acc e = form (text ":get") (expr o EAtom e : map (dotField . unLoc) acc)
    dotField (DotFieldOcc _ (L _ (FieldLabelString f))) = htext f
    recUpdFields = \case
      RegularRecUpdFields _ flds ->
        [ fieldBind (fieldOcc (unLoc l)) r pun | L _ (HsFieldBind _ l r pun) <- flds ]
      OverloadedRecUpdFields _ flds ->
        [ fieldBind (path l) r pun | L _ (HsFieldBind _ (L _ l) r pun) <- flds ]
    path (FieldLabelStrings fs) = case fs of
      f :| [] -> dotField (unLoc f)
      _ -> form (text ":get") (map (dotField . unLoc) (toList fs))
    fieldBind l r pun
      | pun = l
      | otherwise = form (text "=") [l, expr o ETop r]
    arrowHead' (HsModifiedFunArr _ _ a) = case a of
      HsStandardArr _ -> text "->"
      HsLinearArr _ -> text "->."

-- | An SCC label: an identifier label has no source text.
sccLabel :: StringLiteral GhcPs -> SDoc
sccLabel sl = case sl_src sl of
  NoSourceText -> htext (sl_fs sl)
  _ -> stringLit sl

recFields :: PrintOpts -> (LocatedA arg -> SDoc) -> HsRecFields GhcPs (LocatedA arg) -> [SDoc]
recFields _ argDoc (HsRecFields _ flds dotdot) =
  [ if pun then fieldOcc l else form (text "=") [fieldOcc l, argDoc r]
  | L _ (HsFieldBind _ (L _ l) r pun) <- flds ] ++
  [ text ".." | isJust dotdot ]

untypedSplice :: PrintOpts -> HsUntypedSplice GhcPs -> SDoc
untypedSplice o = \case
  HsUntypedSpliceExpr _ e -> form (text ":splice") [expr o EAtom e]
  HsQuasiQuote _ q (L _ txt) ->
    form (text ":qq") [lname q, doubleQuotes (text (escape (unpackHText txt)))]
  where
    escape = concatMap $ \case
      '"' -> "\\\""
      '\\' -> "\\\\"
      '\n' -> "\\n"
      '\t' -> "\\t"
      c -> [c]

-- | An application spine: @(f a b)@, with type arguments as @\@T@.
app :: PrintOpts -> HsExpr GhcPs -> SDoc
app o e0 = case spine e0 [] of
  (L _ (HsVar _ (L _ n)), [Left a]) | isSymName n, rdrNameOcc n /= mkVarOcc "-" ->
    form (rdr n) [expr o EArgLast a]
  (f, args) -> form (fun f) (argDocs args)
  where
    spine e acc = case e of
      HsApp _ (L l f) a -> spine f (Left a : acc) `withLoc` l
      HsAppType _ (L l f) (HsWC _ t) -> spine f (Right t : acc) `withLoc` l
      _ -> (L noSrcSpanA e, acc)
    withLoc r _ = r
    fun (L _ (HsVar _ (L _ n))) = headName n
    fun f = expr o EFun f
    argDocs args = zipWith argDoc [1 :: Int ..] args
      where
        n = length args
        argDoc i = \case
          Left a -> expr o (if i == n then EArgLast else EArg) a
          Right t -> char '@' <> typ o TArg t

-- | An operator chain: @(op a b c)@ or @(:infix a op b op' c)@.
opChain :: PrintOpts -> HsExpr GhcPs -> SDoc
opChain o e0
  | allSame, Just n <- opVar (NE.head ops), isSymName n =
      form (rdr n) (operandDocs)
  | otherwise = form (text ":infix") (interleave operandDocs (map opDoc (toList ops)))
  where
    (operands, ops) = flatten e0
    flatten e = case e of
      OpApp _ (L _ l@OpApp{}) op r ->
        let (os, ps) = flatten l in (os ++ [r], NE.fromList (toList ps ++ [op]))
      OpApp _ l op r -> ([l, r], op :| [])
      _ -> ([], NE.singleton (L noSrcSpanA e))
    n_ops = length operands
    operandDocs = zipWith (\i x -> expr o (posOf i) x) [1 :: Int ..] operands
    posOf i | i == 1 = EOpFirst
            | i == n_ops = EOpLast
            | otherwise = EOpMid
    opVar (L _ (HsVar _ (L _ n))) = Just n
    opVar _ = Nothing
    allSame = case toList ops of
      (op1 : rest) | Just n1 <- opVar op1 -> all (\op -> opVar op == Just n1) rest
      _ -> False
    opDoc (L _ (HsVar _ n)) = lname n
    opDoc (L _ (HsHole (HoleVar n))) = lname n
    opDoc op = expr o ETop op

-- | A case or lambda-case alternative: @(-> pat... rhs)@.
alt :: PrintOpts -> (body -> SDoc) -> Match GhcPs body -> SDoc
alt o bodyDoc (Match _ _ (L _ ps) (GRHSs _ grhss binds)) =
  form (text "->") (map (pat o PTop) ps ++ rhs ++ whereDoc o binds)
  where
    rhs = case grhss of
      L _ (GRHS _ [] b) :| [] -> [bodyDoc b]
      _ -> map (grhs o bodyDoc . unLoc) (toList grhss)

doExpr :: PrintOpts -> HsDoFlavour -> [ExprLStmt GhcPs] -> SDoc
doExpr o flav stmts = case flav of
  DoExpr mm -> formV (qual mm (text "do")) (map (stmt o . unLoc) stmts)
  MDoExpr mm -> formV (qual mm (text "mdo")) (map (stmt o . unLoc) stmts)
  ListComp -> comprehension
  MonadComp -> comprehension
  GhciStmtCtxt -> todo "GhciStmtCtxt"
  where
    qual Nothing d = d
    qual (Just m) d = ppr m <> char '.' <> d
    comprehension = case reverse stmts of
      L _ (LastStmt _ body _ _) : rquals ->
        vec (expr o ETop body : text "|" : concatMap qualDocs (reverse rquals))
      _ -> todo "comprehension"
    qualDocs (L _ s) = case s of
      TransStmt { trS_stmts = prev } -> concatMap qualDocs prev ++ [stmt o s]
      ParStmt _ blocks _ _ ->
        intersperseBar [ map (stmt o . unLoc) ss | ParStmtBlock _ ss _ _ <- toList blocks ]
      _ -> [stmt o s]
    intersperseBar = \case
      [] -> []
      [b] -> b
      (b : bs) -> b ++ [text "|"] ++ intersperseBar bs

stmt :: PrintOpts -> ExprStmt GhcPs -> SDoc
stmt o = \case
  LastStmt _ e _ _ -> expr o ETop e
  BindStmt _ p e -> form (text "<-") [pat o PTop p, expr o ETop e]
  BodyStmt _ e _ _ -> expr o ETop e
  LetStmt _ binds -> formV (text "let") (localBinds o binds)
  ParStmt{} -> todo "ParStmt"
  TransStmt _ form' _ _ using mby _ _ _ -> case form' of
    ThenForm -> form (text "then") ([expr o ETop using] ++ maybe [] (\b -> [text "by", expr o ETop b]) mby)
    GroupForm -> form (text "then group") (maybe [] (\b -> [text "by", expr o ETop b]) mby ++ [text "using", expr o ETop using])
  RecStmt _ (L _ ss) _ _ _ _ _ -> formV (text "rec") (map (stmt o . unLoc) ss)

-------------------------------------------------------------------------------
-- Patterns

pat :: PrintOpts -> PPos -> LPat GhcPs -> SDoc
pat o pos (L _ p) = case p of
  ParPat _ (L _ inner)
    | patNeedsParens (prParens o) pos inner -> patForm o inner
    | otherwise -> form (text ":paren") [pat o PParen (L noSrcSpanA inner)]
  _ -> patForm o p

patForm :: PrintOpts -> Pat GhcPs -> SDoc
patForm o = \case
  WildPat _ -> text "_"
  VarPat _ n -> lname n
  LazyPat _ p -> form (text "~") [pat o PPrefixed p]
  AsPat _ n p -> form (text ":as") [lname n, pat o PPrefixed p]
  ParPat _ p -> form (text ":paren") [pat o PParen p]
  BangPat _ p -> form (text "!") [pat o PPrefixed p]
  ListPat _ ps -> vec (map (pat o PElem) ps)
  TuplePat _ ps boxity -> form (text (if isBoxed boxity then ":tuple" else ":utuple")) (map (pat o PElem) ps)
  OrPat _ ps -> form (text ":or") (map (pat o PTop) (toList ps))
  SumPat _ p tag width ->
    form (text ":usum") (replicate (tag - 1) (text ":_") ++ [pat o PElem p] ++ replicate (width - tag) (text ":_"))
  p@(ConPat _ (L _ con) args) -> case args of
    PrefixCon _ [] -> rdr con
    PrefixCon _ ps -> form (headName con) (map (pat o PArg) ps)
    RecCon _ flds -> form (text ":rec") (rdr con : recFields o (pat o PElem) flds)
    InfixCon{} -> conChain o p
  ViewPat _ e p -> form (text "->") [expr o ETop e, pat o PTop p]
  SplicePat _ sp -> untypedSplice o sp
  LitPat _ l -> hsLit l
  QualLitPat _ ql -> qualLit ql
  NPat _ (L _ ol) mneg _ -> case mneg of
    Nothing -> overLit ol
    Just _ -> form (text "-") [overLit ol]
  NPlusKPat _ n (L _ k) _ _ _ -> form (text "+") [lname n, overLit k]
  SigPat _ p (HsPS _ t) -> form (text "::") [pat o PSigSubj p, typ o TTop t]
  EmbTyPat _ (HsTP _ t) -> form (text "type") [typ o TTop t]
  InvisPat _ (HsTP _ t) -> char '@' <> typ o TArg t
  ModifiedPat _ mods p -> form (text ":mod") (map (modifier o) mods ++ [pat o PTop p])

-- | The operands and constructors of a constructor chain, interleaved.
conChainItems :: PrintOpts -> LPat GhcPs -> [SDoc]
conChainItems o (L _ p0) = interleave (map (pat o POperand) operands) (map rdr cons)
  where
    (operands, cons) = flatten p0
    flatten = \case
      ConPat _ (L _ c) (InfixCon _ (L _ l@(ConPat _ _ InfixCon{})) r) ->
        let (os, cs) = flatten l in (os ++ [r], cs ++ [c])
      ConPat _ (L _ c) (InfixCon _ l r) -> ([l, r], [c])
      _ -> ([], [])

conChain :: PrintOpts -> Pat GhcPs -> SDoc
conChain o p0
  | allSame, (c1 : _) <- cons, isSymName c1 = form (rdr c1) (map (pat o POperand) operands)
  | otherwise = form (text ":infix") (interleave (map (pat o POperand) operands) (map rdr cons))
  where
    (operands, cons) = flatten p0
    flatten = \case
      ConPat _ (L _ c) (InfixCon _ (L _ l@(ConPat _ _ InfixCon{})) r) ->
        let (os, cs) = flatten l in (os ++ [r], cs ++ [c])
      ConPat _ (L _ c) (InfixCon _ l r) -> ([l, r], [c])
      _ -> ([], [])
    allSame = case cons of
      (c1 : rest) -> all (== c1) rest
      [] -> False

-------------------------------------------------------------------------------
-- Arrow commands

cmd :: PrintOpts -> HsCmd GhcPs -> SDoc
cmd o = \case
  HsCmdArrApp _ f a appTy rightToLeft -> case (appTy, rightToLeft) of
    (HsFirstOrderApp, True) -> form (text "-<") [expr o ETop f, expr o ETop a]
    (HsHigherOrderApp, True) -> form (text "-<<") [expr o ETop f, expr o ETop a]
    (HsFirstOrderApp, False) -> form (text ">-") [expr o ETop a, expr o ETop f]
    (HsHigherOrderApp, False) -> form (text ">>-") [expr o ETop a, expr o ETop f]
  HsCmdArrForm _ op fix args -> case (fix, args) of
    (Infix, [L _ (HsCmdTop _ l), L _ (HsCmdTop _ r)]) ->
      form (text ":infix") [lcmd l, expr o ETop op, lcmd r]
    _ -> form (text ":form") (expr o ETop op : [ lcmd c | L _ (HsCmdTop _ c) <- args ])
  HsCmdApp _ c e -> form (lcmd c) [expr o EArgLast e]
  HsCmdLam _ variant (MG _ (L _ ms)) -> case variant of
    LamSingle -> case ms of
      [L _ (Match _ _ (L _ ps) (GRHSs _ (L _ (GRHS _ [] body) :| []) _))] ->
        form (text "\\") (map (pat o PArg) ps ++ [lcmd body])
      _ -> todo "HsCmdLam"
    LamCase -> formV (text "\\case") (map (alt o lcmd . unLoc) ms)
    LamCases -> formV (text "\\cases") (map (alt o lcmd . unLoc) ms)
  HsCmdPar _ c -> form (text ":paren") [lcmd c]
  HsCmdCase _ e (MG _ (L _ ms)) -> formV (text "case" <+> expr o ETop e) (map (alt o lcmd . unLoc) ms)
  HsCmdIf _ _ c t e -> form (text "if") [expr o ETop c, lcmd t, lcmd e]
  HsCmdLet _ binds c -> formV (text "let") (localBinds o binds ++ [lcmd c])
  HsCmdDo _ (L _ stmts) -> formV (text "do") (map (cmdStmt . unLoc) stmts)
  where
    lcmd (L _ c) = cmd o c
    cmdStmt = \case
      LastStmt _ c _ _ -> lcmd c
      BindStmt _ p c -> form (text "<-") [pat o PTop p, lcmd c]
      BodyStmt _ c _ _ -> lcmd c
      LetStmt _ binds -> formV (text "let") (localBinds o binds)
      RecStmt _ (L _ ss) _ _ _ _ _ -> formV (text "rec") (map (cmdStmt . unLoc) ss)
      _ -> todo "cmd stmt"

htext :: HText -> SDoc
htext = text . unpackHText
