{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ghc-lisp parser: forms -> @HsModule GhcPs@ (SPEC.md).
--
-- It produces the same tree as "GHC.Parser" would for the corresponding
-- Haskell text. Where GHC's parser post-processes (declaration heads,
-- constructors, merging function equations, ...), this module calls the
-- same "GHC.Parser.PostProcess" functions, so the results agree.
module GHC.Parser.Lisp
  ( parseLispModule
  , lispHeaderOptions
    -- * Driver hooks
  , isLispFile
  , lispOrHaskell
  , lispOptionsFromFile
  ) where

import GHC.Prelude hiding (head)

import GHC.Hs hiding (patNeedsParens)
import GHC.Parser.Lexer
import GHC.Parser.PostProcess
import GHC.Parser.PostProcess.Haddock (addHaddockToModule)
import GHC.Hs.DocString (mkHsDocStringChunk)
import GHC.Parser (parseIdentifier)
import GHC.Parser.HaddockLex (lexStringLiteral)
import GHC.Parser.Errors.Types
import GHC.Parser.Lisp.Reader
import GHC.Parser.Lisp.Parens
import GHC.Types.Name.Reader
import GHC.Types.Name.Occurrence hiding (isUnderscore)
import GHC.Types.SrcLoc
import GHC.Types.SourceText
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.PkgQual
import GHC.Types.UnresolvedImport (ImportDeclOrigin(..))
import GHC.Types.InlinePragma
import GHC.Unit.Module.Warnings
import GHC.Unit.Module (mkModuleNameFS, ModuleName)
import GHC.Data.FastString
import GHC.Data.OrdList
import GHC.Data.StringBuffer
import GHC.Utils.Error
import GHC.Utils.Outputable hiding (lambda)
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.KnownOccs (consDataCon_RDR)
import GHC.Hs.DocString (HsDocStringDecorator(..))
import Language.Haskell.Syntax.Decls.Overlap
import Language.Haskell.Syntax.Specificity
import Language.Haskell.Syntax.BooleanFormula
import Language.Haskell.Syntax.Text (packHText, unpackHText)

import Control.Monad
import Data.Char (isUpper, isLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromMaybe)
import Data.List (find)

-------------------------------------------------------------------------------
-- Entry points

-- | Parse a ghc-lisp module.
parseLispModule :: ParserOpts -> StringBuffer -> RealSrcLoc
                -> ParseResult (Located (HsModule GhcPs))
parseLispModule opts buf loc =
  unP (go) (initParserState opts buf loc)
  where
    go = case readForms opts buf loc of
      Left (sp, msg) -> failSpan (mkSrcSpanPs sp) msg
      Right (forms, docs) -> do
        -- Haddock comments go where GHC's lexer puts them, and the same
        -- pass as for Haskell attaches them to the tree.
        haddock <- getBit HaddockBit
        when haddock $ P $ \st -> POk st { hdk_comments = hdk_comments st `appOL` toOL (map hdkComment docs) } ()
        moduleP forms >>= addHaddockToModule

-- | Tokens for the opening and closing bracket of a vector form.
bracketTokens :: Form -> (EpToken o, EpToken c, [a])
bracketTokens (Form (PsSpan r (BufSpan b1 b2)) _) =
  ( tok (realSrcSpanStart r) b1
  , tok (mkRealSrcLoc (srcSpanFile r) (srcSpanEndLine r) (srcSpanEndCol r - 1)) (BufPos (bufPos b2 - 1))
  , [] )
  where
    tok l b = EpTok (EpaSpan (mkSrcSpanPs (mkPsSpan (PsLoc l b) (advancePsLoc (PsLoc l b) ' '))))

-- | A ghc-lisp doc comment as the lexer's 'HdkComment'.
hdkComment :: DocComment -> PsLocated HdkComment
hdkComment (DocComment sp kind ls) = L sp $ case kind of
  DocNext -> HdkCommentNext (str HsDocStringNext)
  DocPrev -> HdkCommentPrev (str HsDocStringPrevious)
  DocNamed n -> HdkCommentNamed n (str (HsDocStringNamed n))
  DocSection n -> HdkCommentSection n (str (HsDocStringGroup n))
  where
    str dec = MultiLineDocString noExtField dec
      (NE.fromList [ L (mkSrcSpanPs lsp) (mkHsDocStringChunk t) | (lsp, t) <- ls ])

-- | Header options of a ghc-lisp file: @(:language ...)@ and
-- @(:options-ghc ...)@ forms before everything else, as GHC flags
-- (@-XGADTs@, @-Wall@). Used by the downsweep.
lispHeaderOptions :: ParserOpts -> StringBuffer -> RealSrcLoc -> [Located String]
lispHeaderOptions opts buf loc = concatMap opt (readFormsWhile isHeader opts buf loc)
  where
    isHeader f = case formNode f of
      FList (Form _ (FKeyword k) : _) -> k `elem` map fsLit ["language", "options-ghc", "options-haddock"]
      _ -> False
    opt f = case formNode f of
      FList (Form _ (FKeyword k) : args)
        | k == fsLit "language" ->
            [ L (formSpan a) ("-X" ++ unpackFS s) | a <- args, Just s <- [plainName a] ]
        | k == fsLit "options-ghc" ->
            [ L (formSpan a) w | a@(Form _ (FAtom (ITstring _ _ s))) <- args
                               , w <- words (unpackHTextS s) ]
      _ -> []
    unpackHTextS = unpackFS . mkFastStringShortText

-------------------------------------------------------------------------------
-- Driver hooks (each called from a one-line `ghc-lisp:` hook upstream)

-- | Is this a ghc-lisp source file? A boot file counts too: the finder
-- looks for @M.hsl-boot@ beside @M.hsl@.
isLispFile :: FilePath -> Bool
isLispFile f = ".hsl" `isSuffixOf` f || ".hsl-boot" `isSuffixOf` f

-- | Run GHC's parser, or the ghc-lisp parser for a @.hsl@ source file.
-- Hook in "GHC.Driver.Main.Passes" (the module parse) and
-- "GHC.Parser.Header" (the downsweep's header parse).
lispOrHaskell :: Maybe FilePath -> P (Located (HsModule GhcPs)) -> PState
              -> ParseResult (Located (HsModule GhcPs))
lispOrHaskell (Just file) _ pst
  | isLispFile file = parseLispModule (options pst) (buffer pst) (psRealLoc (loc pst))
lispOrHaskell _ p pst = unP p pst

-- | The header options of a @.hsl@ file, as 'getOptionsFromFile' returns
-- them for Haskell files. CPP is not supported in @.hsl@ files (DESIGN.md
-- D11).
lispOptionsFromFile :: ParserOpts -> FilePath -> IO (Messages PsMessage, [Located String])
lispOptionsFromFile opts file = do
  buf <- hGetStringBuffer file
  let os = lispHeaderOptions opts buf (mkRealSrcLoc (mkFastString file) 1 1)
  -- CPP is left out here and reported by the parser (moduleP).
  pure (emptyMessages, filter (\(L _ o) -> o `notElem` ["-XCPP", "-cpp"]) os)

-------------------------------------------------------------------------------
-- Errors and locations

failSpan :: SrcSpan -> String -> P a
failSpan sp msg = addFatalError $ mkPlainErrorMsgEnvelope sp $
  PsUnknownMessage $ mkSimpleUnknownDiagnostic $ mkPlainError noHints (text msg)

failAt :: Form -> String -> P a
failAt f = failSpan (formSpan f)

at :: NoAnn ann => Form -> a -> GenLocated (EpAnn ann) a
at f = L (noAnnSrcSpan (formSpan f))

atSpan :: NoAnn ann => SrcSpan -> a -> GenLocated (EpAnn ann) a
atSpan sp = L (noAnnSrcSpan sp)

-- | A context is located at its constraints, as in GHC (Haddock comments
-- after the context belong to the type, not to the context); an empty
-- context at the => head.
ctxAt :: NoAnn ann => Form -> [Form] -> a -> GenLocated (EpAnn ann) a
ctxAt hd [] = at hd
ctxAt _ cs = atSpan (spanOf cs)

spanOf :: [Form] -> SrcSpan
spanOf [] = noSrcSpan
spanOf (x : xs) = foldl combineSrcSpans (formSpan x) (map formSpan xs)

parenOptsP :: P ParenOpts
parenOptsP = ParenOpts <$> getBit BlockArgumentsBit
                       <*> (not <$> getBit NoLexicalNegationBit)
                       <*> getBit NegativeLiteralsBit

-- | Bodies of matches and guarded right-hand sides: expressions or commands.
type LBody body =
  ( Anno (GRHS GhcPs (LocatedA (body GhcPs))) ~ EpAnnCO
  , Anno (Match GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA
  , Anno [LocatedA (Match GhcPs (LocatedA (body GhcPs)))] ~ SrcSpanAnnA )

-------------------------------------------------------------------------------
-- Form views

-- | A symbol: one Haskell name lexeme, possibly qualified.
data Sym = Sym { symMod :: Maybe FastString, symText :: FastString, symKind :: SymKind }

data SymKind = VarId | ConId | VarSym | ConSym
  deriving Eq

symOf :: Form -> Maybe Sym
symOf (Form _ (FAtom t)) = tokSym t
symOf _ = Nothing

tokSym :: Token -> Maybe Sym
tokSym = \case
  ITvarid s -> var s
  ITconid s -> Just (Sym Nothing s ConId)
  ITvarsym s -> Just (Sym Nothing s VarSym)
  ITconsym s -> Just (Sym Nothing s ConSym)
  ITqvarid (m, s) -> Just (Sym (Just m) s VarId)
  ITqconid (m, s) -> Just (Sym (Just m) s ConId)
  ITqvarsym (m, s) -> Just (Sym (Just m) s VarSym)
  ITqconsym (m, s) -> Just (Sym (Just m) s ConSym)
  ITminus -> vsym "-"
  ITprefixminus -> vsym "-"
  ITbang -> vsym "!"
  ITtilde -> vsym "~"
  ITstar _ -> vsym "*"
  ITdot -> vsym "."
  ITpercent -> vsym "%"
  ITcolon -> Just (Sym Nothing (fsLit ":") ConSym)
  -- special identifiers that are ordinary variables outside their contexts
  ITas -> varS "as"
  ITqualified -> varS "qualified"
  IThiding -> varS "hiding"
  ITexport -> varS "export"
  ITlabel -> varS "label"
  ITdynamic -> varS "dynamic"
  ITsafe -> varS "safe"
  ITinterruptible -> varS "interruptible"
  ITunsafe -> varS "unsafe"
  ITstdcallconv -> varS "stdcall"
  ITccallconv -> varS "ccall"
  ITcapiconv -> varS "capi"
  ITprimcallconv -> varS "prim"
  ITjavascriptcallconv -> varS "javascript"
  ITfamily -> varS "family"
  ITrole -> varS "role"
  ITgroup -> varS "group"
  ITby -> varS "by"
  ITusing -> varS "using"
  ITpattern -> varS "pattern"
  ITstock -> varS "stock"
  ITanyclass -> varS "anyclass"
  ITvia -> varS "via"
  ITunit -> varS "unit"
  ITsignature -> varS "signature"
  ITdependency -> varS "dependency"
  ITrequires -> varS "requires"
  ITsplice -> varS "splice"
  ITquote -> varS "quote"
  _ -> Nothing
  where
    var s = Just (Sym Nothing s VarId)
    varS s = var (fsLit s)
    vsym s = Just (Sym Nothing (fsLit s) VarSym)

-- | A plain (unqualified) name's text.
plainName :: Form -> Maybe FastString
plainName f = case symOf f of
  Just (Sym Nothing s _) -> Just s
  _ -> Nothing

isSymbolic :: Sym -> Bool
isSymbolic s = symKind s `elem` [VarSym, ConSym]

-- | The head of a list form.
data Head
  = HKeyword FastString       -- ^ @:name@
  | HTok Token                -- ^ a reserved word or reserved operator
  | HSym Sym                  -- ^ an ordinary name or operator
  | HOther                    -- ^ any other form

headOf :: Form -> Head
headOf (Form _ n) = case n of
  FKeyword k -> HKeyword k
  FAtom t | Just s <- tokSym t -> HSym s
          | otherwise -> HTok t
  _ -> HOther

-- | @(:name x)@, @(:tuple-con n)@, @(:utuple-con n)@.
isNameForm :: Form -> Bool
isNameForm (Form _ (FList [h, _])) = isKw "name" h || isKw "tuple-con" h || isKw "utuple-con" h || isKw "usum-con" h
isNameForm (Form _ (FList [h, _, _])) = isKw "usum-con" h
isNameForm _ = False

isKw :: String -> Form -> Bool
isKw k (Form _ (FKeyword k')) = k' == fsLit k
isKw _ _ = False

isTok :: (Token -> Bool) -> Form -> Bool
isTok p (Form _ (FAtom t)) = p t
isTok _ _ = False

isName :: String -> Form -> Bool
isName s f = plainName f == Just (fsLit s)

isDotDot :: Form -> Bool
isDotDot = isTok (\case ITdotdot -> True; _ -> False)

isBar :: Form -> Bool
isBar = isTok (\case ITvbar -> True; _ -> False)

isRArrow :: Form -> Bool
isRArrow = isTok (\case ITrarrow _ -> True; _ -> False)

isLArrow :: Form -> Bool
isLArrow = isTok (\case ITlarrow _ -> True; _ -> False)

isEqual :: Form -> Bool
isEqual = isTok (\case ITequal -> True; _ -> False)

isDcolon :: Form -> Bool
isDcolon = isTok (\case ITdcolon _ -> True; _ -> False)

isUnderscore :: Form -> Bool
isUnderscore = isTok (\case ITunderscore -> True; _ -> False)

isHole :: Form -> Bool
isHole = isKw "_"

listOf :: Form -> Maybe [Form]
listOf (Form _ (FList fs)) = Just fs
listOf _ = Nothing

-- | A list form headed by the given keyword.
kwForm :: String -> Form -> Maybe [Form]
kwForm k (Form _ (FList (h : args))) | isKw k h = Just args
kwForm _ _ = Nothing

-- | A list form headed by a token.
tokForm :: (Token -> Bool) -> Form -> Maybe [Form]
tokForm p (Form _ (FList (h : args))) | isTok p h = Just args
tokForm _ _ = Nothing

-------------------------------------------------------------------------------
-- Names

data NS = NSExpr | NSType

rdrFor :: NS -> Sym -> RdrName
rdrFor ns (Sym mm s k)
  | Nothing <- mm, s == fsLit ":" = consDataCon_RDR
  | otherwise = maybe (mkUnqual space s) (\m -> mkQual space (m, s)) mm
  where
    space = case (ns, k) of
      (NSExpr, VarId) -> varName
      (NSExpr, VarSym) -> varName
      (NSExpr, ConId) -> dataName
      (NSExpr, ConSym) -> dataName
      (NSType, VarId) -> tvName
      (NSType, _) -> tcClsName

nameAt :: NS -> Form -> P (LocatedN RdrName)
nameAt ns f = case f of
  _ | Just s <- symOf f -> pure (at f (rdrFor ns s))
  Form _ (FList [h, x]) | isKw "name" h -> nameAt' x
  Form _ (FList [h, n]) | isKw "tuple-con" h -> tupleCon Boxed n
  Form _ (FList [h, n]) | isKw "utuple-con" h -> tupleCon Unboxed n
  Form _ (FList [h, n]) | isKw "usum-con" h, Form _ (FAtom (ITinteger il)) <- n ->
    pure (at f (getRdrName (sumTyCon (fromIntegral (il_value il)))))
  Form _ (FList [h, Form _ (FAtom (ITinteger alt)), Form _ (FAtom (ITinteger ar))]) | isKw "usum-con" h ->
    pure (at f (getRdrName (sumDataCon (fromIntegral (il_value alt)) (fromIntegral (il_value ar)))))
  Form _ (FList []) -> pure (at f (byNS (getRdrName unitDataCon) (getRdrName unitTyCon)))
  Form _ (FVector []) -> pure (at f (byNS (getRdrName nilDataCon) (getRdrName listTyCon)))
  _ -> failAt f "expected a name"
  where
    byNS e t = case ns of
      NSExpr -> e
      NSType -> t
    nameAt' x
      | Just s <- symOf x = pure (at f (rdrFor ns s))
      | isRArrow x = pure (at f (getRdrName unrestrictedFunTyCon))
      | isTok (\case ITtilde -> True; _ -> False) x = pure (at f (rdrFor ns (Sym Nothing (fsLit "~") VarSym)))
      | otherwise = failAt x "expected a name after :name"
    tupleCon b n = case n of
      Form _ (FAtom (ITinteger il)) ->
        let arity = fromIntegral (il_value il)
        in case ns of
             NSExpr -> pure (at f (getRdrName (tupleDataCon b arity)))
             -- the tuple type constructor honours ListTuplePuns
             NSType -> at f <$> mkTupleSyntaxTycon b arity
      _ -> failAt n "expected an arity"

varName' :: Form -> P (LocatedN RdrName)
varName' f = case symOf f of
  Just s | symKind s `elem` [VarId, VarSym] -> nameAt NSExpr f
  _ -> case listOf f of
    Just [h, _] | isKw "name" h -> nameAt NSExpr f
    _ -> failAt f "expected a variable name"

conName' :: NS -> Form -> P (LocatedN RdrName)
conName' = nameAt

-------------------------------------------------------------------------------
-- Module

moduleP :: [Form] -> P (Located (HsModule GhcPs))
moduleP forms0 = do
  let forms1 = dropWhile isHeaderPragma forms0
  forM_ (takeWhile isHeaderPragma forms0) $ \h -> forM_ (fromMaybe [] (listOf h)) $ \x ->
    when (isName "CPP" x || isTok (\case ITstring _ _ s -> "-cpp" `elem` words (unpackHText s); _ -> False) x) $
      failAt x "CPP is not supported in .hsl files"
  (name, warn, exports, exportsF, rest) <- case forms1 of
    (f : fs) | Just args <- tokForm (\case ITmodule -> True; _ -> False) f -> do
      (n, w, e) <- moduleHead f args
      pure (Just n, w, e, find isVector args, fs)
    fs -> pure (Nothing, Nothing, Nothing, Nothing, fs)
  let (importForms, declForms) = span isImport rest
  imports <- mapM importDecl importForms
  decls <- concat <$> mapM (topDecl TopCtx) declForms
  let sp = spanOf forms0
  pure $ L sp HsModule
    { hsmodExt = XModulePs
        -- The export list's brackets delimit its Haddock comments.
        { hsmodAnn = EpAnn (spanAsAnchor sp)
            (noAnn { am_exports = maybe (noEpTok, noEpTok, []) bracketTokens exportsF })
            emptyComments
        , hsmodLayout = EpNoLayout
        , hsmodDeprecMessage = warn
        , hsmodHaddockModHeader = Nothing }
    , hsmodName = name
    , hsmodExports = exports
    , hsmodImports = imports
    , hsmodDecls = cvTopDecls (toOL decls)
    }
  where
    isVector (Form _ (FVector _)) = True
    isVector _ = False
    isHeaderPragma f = any (\k -> isJust (kwForm k f)) ["language", "options-ghc", "options-haddock"]
    isImport f = isJust (tokForm (\case ITimport -> True; _ -> False) f)

moduleHead :: Form -> [Form] -> P (LocatedA ModuleName, Maybe (LWarningTxt GhcPs), Maybe [LIE GhcPs])
moduleHead f = \case
  (n : rest) -> do
    mn <- moduleName n
    (w, rest') <- case rest of
      (x : xs) | isJust (kwForm "deprecated" x) || isJust (kwForm "warning" x) -> do
        w <- warningTxtP x
        pure (Just w, xs)
      _ -> pure (Nothing, rest)
    ex <- case rest' of
      [] -> pure Nothing
      [Form _ (FVector items)] -> Just <$> mapM (ieP True) items
      (x : _) -> failAt x "unexpected form in the module header"
    pure (mn, w, ex)
  [] -> failAt f "a module form needs a name"

moduleName :: Form -> P (LocatedA ModuleName)
moduleName f = case f of
  Form _ (FAtom (ITconid s)) -> pure (at f (mkModuleNameFS s))
  Form _ (FAtom (ITqconid (m, s))) -> pure (at f (mkModuleNameFS (concatFS [m, fsLit ".", s])))
  _ -> failAt f "expected a module name"

-------------------------------------------------------------------------------
-- Imports and exports

importDecl :: Form -> P (LImportDecl GhcPs)
importDecl f = do
  let args = fromMaybe [] (tokForm (\case ITimport -> True; _ -> False) f)
  let (src, a1) = flag (isKw "source") args
      (safe, a2) = flag (isName "safe") a1
      (lvlPre, a3) = level a2
      (qualPre, a4) = flag (isName "qualified") a3
      (pkg, a5) = case a4 of
        (Form _ (FAtom (ITstring st _ s)) : r) -> (RawPkgQual st (mkFastStringShortText s), r)
        r -> (NoRawPkgQual, r)
  (mn, a6) <- case a5 of
    (n : r) -> (,r) <$> moduleName n
    [] -> failAt f "an import needs a module name"
  let (lvlPost, a7) = level a6
      (qualPost, a8) = flag (isName "qualified") a7
  (as, a9) <- case a8 of
    (a : n : r) | isName "as" a -> do { m <- moduleName n; pure (Just m, r) }
    r -> pure (Nothing, r)
  list <- case a9 of
    [] -> pure Nothing
    [Form _ (FVector items)] -> do
      ies <- mapM (ieP False) items
      pure (Just (Exactly, ies))
    [h, Form _ (FVector items)] | isName "hiding" h -> do
      ies <- mapM (ieP False) items
      pure (Just (EverythingBut, ies))
    (x : _) -> failAt x "unexpected form in an import"
  pure $ at f ImportDecl
    { ideclExt = XImportDeclPass noAnn (if src then SourceText (fsLit "{-# SOURCE") else NoSourceText) UserWrittenImport
    , ideclName = mn
    , ideclPkgQual = pkgQual pkg
    , ideclSource = if src then IsBoot else NotBoot
    , ideclLevelSpec = case (lvlPre, lvlPost) of
        (Just l, _) -> LevelStylePre l
        (_, Just l) -> LevelStylePost l
        _ -> NotLevelled
    , ideclSafe = safe
    , ideclQualified = if | qualPre -> QualifiedPre
                          | qualPost -> QualifiedPost
                          | otherwise -> NotQualified
    , ideclAs = as
    , ideclImportList = list
    }
  where
    flag p (x : r) | p x = (True, r)
    flag _ r = (False, r)
    level (x : r)
      | isName "splice" x || isTok (\case ITsplice -> True; _ -> False) x = (Just ImportDeclSplice, r)
      | isName "quote" x || isTok (\case ITquote -> True; _ -> False) x = (Just ImportDeclQuote, r)
    level r = (Nothing, r)
    pkgQual = id

-- | An export or import item.
ieP :: Bool -> Form -> P (LIE GhcPs)
ieP isExport f = case f of
  Form _ (FList (h : args))
    | isKw "deprecated" h || isKw "warning" h, not (null args) -> do
        let (wargs, item) = (init args, last args)
        w <- warningTxtP (Form (formPsSpan f) (FList (h : wargs)))
        L l ie' <- ieP isExport item
        pure (L l (setWarning (Just w) ie'))
    | isTok (\case ITmodule -> True; _ -> False) h, [m] <- args -> do
        mn <- moduleName m
        pure (at f (IEModuleContents (Nothing, noAnn) mn))
    | Just ns <- namespaceOf h, [x] <- args, isDotDot x ->
        pure (at f (IEWholeNamespace (IEWholeNamespaceExt Nothing noAnn []) ns))
    | Just ns <- namespaceOf h, [Form _ (FList (n : subs))] <- args, all isDotDot subs, not (null subs) -> do
        nm <- wrapped n
        pure (at f (IEThingAll (IEThingAllExt Nothing noAnn noAnn noAnn) ns nm Nothing))
  Form _ (FList [h, x]) | isWrapHead h, isJust (symOf x) -> plainItem
  Form _ (FList (n : subs)) | not (isNamespaceHead n) -> do
    nm <- wrapped n
    case subs of
      [d] | isDotDot d -> pure (at f (IEThingAll (IEThingAllExt Nothing noAnn noAnn noAnn) (NoNamespaceSpecifier noExtField) nm Nothing))
      [u] | isKw "_" u -> pure (at f (IEThingWith (Nothing, noAnn) nm NoIEWildcard [] Nothing))
      _ -> do
        let (before, after) = break isDotDot subs
        items <- mapM wrapped (before ++ drop 1 after)
        let wc = if null after then NoIEWildcard else IEWildcard (length before)
        pure (at f (IEThingWith (Nothing, noAnn) nm wc items Nothing))
  _ | isDotDot f -> pure (at f (IEWholeNamespace (IEWholeNamespaceExt Nothing noAnn []) (NoNamespaceSpecifier noExtField)))
    | otherwise -> plainItem
  where
    _ = isExport
    plainItem = do
      nm@(L _ w) <- wrapped f
      let isVar = case w of
            IEName _ (L _ r) -> isVarOcc (rdrNameOcc r)
            IEPattern{} -> True
            IEData _ (L _ r) -> isVarOcc (rdrNameOcc r)
            _ -> False
      pure (at f (if isVar then IEVar Nothing nm Nothing else IEThingAbs Nothing nm Nothing))
    isWrapHead h = isName "pattern" h || isTok (\case ITpattern -> True; ITtype -> True; ITdata -> True; ITdefault -> True; _ -> False) h
    isNamespaceHead h = isJust (namespaceOf h) || isTok (\case ITmodule -> True; _ -> False) h
                        || isKw "deprecated" h || isKw "warning" h
    namespaceOf :: Form -> Maybe (NamespaceSpecifier GhcPs)
    namespaceOf h
      | isTok (\case ITtype -> True; _ -> False) h = Just (TypeNamespaceSpecifier noAnn)
      | isTok (\case ITdata -> True; _ -> False) h = Just (DataNamespaceSpecifier noAnn)
      | otherwise = Nothing
    wrapped :: Form -> P (LIEWrappedName GhcPs)
    wrapped x = case x of
      Form _ (FList [h, n])
        | isTok (\case ITtype -> True; _ -> False) h -> at x . IEType noAnn <$> nameAt NSType n
        | isTok (\case ITdata -> True; _ -> False) h -> at x . IEData noAnn <$> nameAt NSExpr n
        | isName "pattern" h -> at x . IEPattern noAnn <$> nameAt NSExpr n
        | isTok (\case ITdefault -> True; _ -> False) h -> at x . IEDefault noAnn <$> nameAt NSType n
      _ -> do
        ns <- case symOf x of
          Just s | symKind s `elem` [VarId, VarSym] -> pure NSExpr
          _ -> pure NSType
        at x . IEName noExtField <$> nameAt ns x
    setWarning w = \case
      IEVar _ n d -> IEVar w n d
      IEThingAbs _ n d -> IEThingAbs w n d
      IEThingAll e ns n d -> IEThingAll e { ieta_warning = w } ns n d
      IEThingWith (_, a) n wc ns d -> IEThingWith (w, a) n wc ns d
      IEModuleContents (_, a) m -> IEModuleContents (w, a) m
      IEWholeNamespace e ns -> IEWholeNamespace e { iewn_warning = w } ns
      other -> other

-------------------------------------------------------------------------------
-- Warnings

warningTxtP :: Form -> P (LWarningTxt GhcPs)
warningTxtP f = case listOf f of
  Just (h : args)
    | isKw "deprecated" h, [m] <- args -> at f . DeprecatedTxt noAnn <$> msgs m
    | isKw "warning" h -> case args of
        [i, c, m] | isTok (\case ITin -> True; _ -> False) i -> do
          cat <- category c
          at f . WarningTxt noAnn (Just cat) <$> msgs m
        [m] -> at f . WarningTxt noAnn Nothing <$> msgs m
        _ -> failAt f "malformed :warning"
  _ -> failAt f "expected (:deprecated ...) or (:warning ...)"
  where
    category c = case c of
      Form _ (FAtom (ITstring st _ s)) ->
        pure (at c (InWarningCategory (noAnn, st) (at c (WarningCategory s))))
      _ -> failAt c "expected a warning category string"
    msgs m = case m of
      Form _ (FVector ms) -> mapM msg ms
      _ -> (: []) <$> msg m
    msg m = case m of
      Form _ (FAtom (ITstring st _ s)) ->
        pure (reLoc (lexStringLiteral parseIdentifier (L (formSpan m) (StringLiteral st s))))
      _ -> failAt m "expected a string"

-------------------------------------------------------------------------------
-- Declarations

data DeclCtx = TopCtx | ClassCtx | InstCtx | LocalCtx
  deriving Eq

topDecl :: DeclCtx -> Form -> P [LHsDecl GhcPs]
topDecl ctx f = case f of
  Form _ (FList (h : args)) -> case headOf h of
    HTok ITequal -> (: []) <$> bindDecl ctx f args
    HTok (ITdcolon _) -> (: []) <$> sigDecl ctx f args
    HTok ITdata -> one (dataDecl ctx f DataType False args)
    HTok ITnewtype -> one (dataDecl ctx f NewType False args)
    HTok ITtype -> typeDecl ctx f args
    HTok ITclass -> one (classDecl f args)
    HTok ITinstance -> one (instanceDecl f args)
    HTok ITderiving -> one (standaloneDeriving f args)
    HTok ITdefault
      | ctx == ClassCtx, [s] <- args -> one (defaultSig s)
      | otherwise -> one (defaultDecl [] f args)
    HTok ITforeign -> one (foreignDecl [] f args)
    HTok ITinfixl -> one (fixityDecl InfixL f args)
    HTok ITinfixr -> one (fixityDecl InfixR f args)
    HTok ITinfix -> one (fixityDecl InfixN f args)
    HTok ITpattern -> one (patSynDecl f args)
    HSym s | symText s == fsLit "pattern", Nothing <- symMod s -> one (patSynDecl f args)
    HKeyword k -> keywordDecl ctx f (unpackFS k) args
    _ | ctx == TopCtx -> do
          e <- expr ETop f
          pure [at f (SpliceD noExtField (SpliceDecl noExtField (at f (HsUntypedSpliceExpr noAnn e)) BareSplice))]
      | otherwise -> failAt f "unexpected form in declarations"
  Form _ (FAtom _) | ctx == TopCtx -> do
    e <- expr ETop f
    pure [at f (SpliceD noExtField (SpliceDecl noExtField (at f (HsUntypedSpliceExpr noAnn e)) BareSplice))]
  _ -> failAt f "expected a declaration"
  where
    one p = (: []) <$> p
    defaultSig s = case listOf s of
      Just (h : rest) | isDcolon h -> do
        (names, t) <- sigParts s rest
        pure (at f (SigD noExtField (ClassOpSig noAnn True names (hsTypeToHsSigType t))))
      _ -> failAt s "expected (default (:: name type))"

keywordDecl :: DeclCtx -> Form -> String -> [Form] -> P [LHsDecl GhcPs]
keywordDecl ctx f k args = case k of
  "mod" | not (null args) -> do
    mods <- mapM modifierP (init args)
    ds <- topDecl ctx (last args)
    pure [ L l (addMods mods d) | L l d <- ds ]
  "splice" | [e] <- args -> do
    sp <- untypedSpliceExpr f e
    pure [at f (SpliceD noExtField (SpliceDecl noExtField (at f sp) DollarSplice))]
  "qq" -> do
    sp <- quasiQuote f args
    pure [at f (SpliceD noExtField (SpliceDecl noExtField (at f sp) DollarSplice))]
  "deprecated" -> warnDecl
  "warning" -> warnDecl
  "ann" -> (: []) <$> annDecl f args
  "rules" -> do
    rs <- mapM ruleDecl args
    pure [at f (RuleD noExtField (HsRules (noAnn, NoSourceText) rs))]
  "inline" -> inlineSig Inline
  "noinline" -> inlineSig NoInline
  "inlinable" -> inlineSig Inlinable
  "inlineable" -> inlineSig Inlinable
  "opaque" -> case args of
    [n] -> do
      nm <- nameAt NSExpr n
      pure [sigD (InlineSig noAnn nm (mkOpaquePragma NoSourceText))]
    _ -> failAt f "expected (:opaque name)"
  "specialise" -> specSig
  "specialize" -> specSig
  "minimal" | [bf] <- args -> do
    b <- boolFormula bf
    pure [sigD (MinimalSig (noAnn, NoSourceText) b)]
  "complete" -> do
    let (names, rest) = break isDcolon args
    ns <- mapM (nameAt NSExpr) names
    ty <- case rest of
      [_, t] -> Just <$> nameAt NSType t
      [] -> pure Nothing
      _ -> failAt f "malformed :complete"
    pure [sigD (CompleteMatchSig (noAnn, NoSourceText) ns ty)]
  "scc" -> case args of
    [n] -> do { nm <- varName' n; pure [sigD (SCCFunSig (noAnn, NoSourceText) nm Nothing)] }
    [n, l@(Form _ (FAtom (ITstring st _ s)))] -> do
      nm <- varName' n
      pure [sigD (SCCFunSig (noAnn, NoSourceText) nm (Just (at l (StringLiteral st s))))]
    _ -> failAt f "malformed :scc"
  _ | ctx == TopCtx -> do
        e <- expr ETop f
        pure [at f (SpliceD noExtField (SpliceDecl noExtField (at f (HsUntypedSpliceExpr noAnn e)) BareSplice))]
    | otherwise -> failAt f ("unknown declaration form :" ++ k)
  where
    sigD s = at f (SigD noExtField s)
    warnDecl = do
      -- (:deprecated ns? names... msg), (:warning in? "cat"? ns? names... msg)
      let isW = k == "warning"
      (cat, rest0) <- case args of
        (i : c : r) | isW, isTok (\case ITin -> True; _ -> False) i -> pure (Just c, r)
        r -> pure (Nothing, r)
      when (null rest0) $ failAt f "missing message"
      let (pre, msgF) = (init rest0, last rest0)
          (ns, names) = case pre of
            (h : r) | isTok (\case ITtype -> True; _ -> False) h -> (TypeNamespaceSpecifier noAnn, r)
                    | isTok (\case ITdata -> True; _ -> False) h -> (DataNamespaceSpecifier noAnn, r)
            r -> (NoNamespaceSpecifier noExtField, r)
      w <- warningTxtP (Form (formPsSpan f) (FList ([Form (formPsSpan f) (FKeyword (fsLit k))]
                                               ++ maybe [] (\c -> [Form (formPsSpan f) (FAtom ITin), c]) cat ++ [msgF])))
      ns' <- mapM (nameAt NSExpr) names
      pure [at f (WarningD noExtField (Warnings (noAnn, NoSourceText) [at f (Warning noAnn ns ns' (unLoc w))]))]
    inlineSig spec = do
      let (act, rest) = activationP args
          (conlike, rest') = case rest of
            (c : r) | isKw "conlike" c -> (True, r)
            r -> (False, r)
      case rest' of
        [n] -> do
          nm <- nameAt NSExpr n
          a <- act
          pure [sigD (InlineSig noAnn nm (mkInlinePragma NoSourceText (spec, if conlike then ConLike else FunLike) a))]
        _ -> failAt f "expected (:inline [phase]? :conlike? name)"
    specSig = do
      let (inl, r0) = case args of
            (c : r) | isKw "inline" c -> (Inline, r)
                    | isKw "noinline" c -> (NoInline, r)
            r -> (NoUserInlinePrag, r)
          (act, r1) = activationP r0
      a <- act
      let prag = mkInlinePragma NoSourceText (inl, FunLike) a
      case r1 of
        [i, t] | isTok (\case ITinstance -> True; _ -> False) i -> do
          ty <- sigTypeP t
          pure [sigD (SpecInstSig (noAnn, NoSourceText) ty)]
        (n : tys@(_ : _)) | isJust (symOf n) -> do
          nm <- varName' n
          ts <- mapM sigTypeP tys
          pure [sigD (SpecSig noAnn nm ts prag)]
        _ -> do
          let (bndrForms, rest) = span (isJust . tokForm (\case ITforall _ -> True; _ -> False)) r1
          bndrs <- ruleBndrsP bndrForms
          case rest of
            [e] -> do
              ex <- expr ETop e
              pure [sigD (SpecSigE noAnn bndrs ex prag)]
            _ -> failAt f "malformed :specialise"

addMods :: [LHsModifier GhcPs] -> HsDecl GhcPs -> HsDecl GhcPs
addMods mods = \case
  TyClD x d@DataDecl{} -> TyClD x d { tcdModifiers = mods }
  TyClD x d@ClassDecl{} -> TyClD x d { tcdModifiers = mods }
  InstD x (ClsInstD y d) -> InstD x (ClsInstD y d { cid_modifiers = mods })
  SigD x (TypeSig a _ ns t) -> SigD x (TypeSig a mods ns t)
  DefD x d -> DefD x d { defd_modifiers = mods }
  ForD x d@ForeignImport{} -> ForD x d { fd_modifiers = mods }
  ForD x d@ForeignExport{} -> ForD x d { fd_modifiers = mods }
  ValD x b@PatBind{} -> ValD x b { pat_mods = mods }
  other -> other

modifierP :: Form -> P (LHsModifier GhcPs)
modifierP f = at f . HsModifier noAnn <$> typ TArg f

activationP :: [Form] -> (P (Maybe ActivationGhc), [Form])
activationP = \case
  (v@(Form _ (FVector items)) : r) -> (Just <$> act v items, r)
  r -> (pure Nothing, r)
  where
    act v = \case
      [Form _ (FAtom (ITinteger il))] -> pure (ActiveAfter (fromIntegral (il_value il)))
      [Form _ (FAtom t)] | isTilde t -> pure NeverActive
      [Form _ (FAtom t), Form _ (FAtom (ITinteger il))] | isTilde t -> pure (ActiveBefore (fromIntegral (il_value il)))
      [Form _ (FList [Form _ (FAtom t), Form _ (FAtom (ITinteger il))])] | isTilde t -> pure (ActiveBefore (fromIntegral (il_value il)))
      _ -> failAt v "expected an activation: [2], [~2] or [~]"
    isTilde = \case
      ITtilde -> True
      ITvarsym s -> s == fsLit "~"
      _ -> False

boolFormula :: Form -> P (LBooleanFormula GhcPs)
boolFormula f = case f of
  Form _ (FList (h : args))
    | isKw "or" h -> at f . Or noExtField <$> mapM boolFormula args
    | isKw "and" h -> at f . And noExtField <$> mapM boolFormula args
    | isKw "paren" h, [x] <- args -> at f . Parens noAnn <$> boolFormula x
  _ -> at f . Var noExtField <$> varName' f

annDecl :: Form -> [Form] -> P (LHsDecl GhcPs)
annDecl f = \case
  [m, e] | isTok (\case ITmodule -> True; _ -> False) m -> mk ModuleAnnProvenance e
  [t, n, e] | isTok (\case ITtype -> True; _ -> False) t -> do
    nm <- nameAt NSType n
    mk (TypeAnnProvenance nm) e
  [n, e] -> do
    nm <- nameAt NSExpr n
    mk (ValueAnnProvenance nm) e
  _ -> failAt f "malformed :ann"
  where
    mk prov e = do
      ex <- expr ETop e
      pure (at f (AnnD noExtField (HsAnnotation (noAnn, NoSourceText) prov ex)))

ruleDecl :: Form -> P (LRuleDecl GhcPs)
ruleDecl f = case listOf f of
  Just (nameF@(Form _ (FAtom (ITstring nameSrc _ name))) : rest0) -> do
    let (act, rest1) = activationP rest0
        (bndrForms, rest2) = span (isJust . tokForm (\case ITforall _ -> True; _ -> False)) rest1
    a <- act
    bndrs <- ruleBndrsP bndrForms
    case rest2 of
      [eq] | Just [l, r] <- tokForm (\case ITequal -> True; _ -> False) eq -> do
        lhs <- expr ETop l
        rhs <- expr ETop r
        pure (at f (HsRule (noAnn, nameSrc) (at nameF name) (fromMaybe AlwaysActive a) bndrs lhs rhs))
      _ -> failAt f "expected (= lhs rhs) in a rule"
  _ -> failAt f "expected (\"name\" ... (= lhs rhs))"

ruleBndrsP :: [Form] -> P (RuleBndrs GhcPs)
ruleBndrsP = \case
  [] -> pure (RuleBndrs noAnn Nothing [])
  [tm] -> RuleBndrs noAnn Nothing <$> terms tm
  [ty, tm] -> do
    tvs <- tyVars ty
    RuleBndrs noAnn (Just tvs) <$> terms tm
  (x : _) -> failAt x "at most two foralls"
  where
    args x = fromMaybe [] (tokForm (\case ITforall _ -> True; _ -> False) x)
    tyVars x = mapM (tyVarBndrP (const (pure ()))) (args x)
    terms x = forM (args x) $ \b -> case listOf b of
      Just [h, n, t] | isDcolon h -> do
        nm <- varName' n
        ty <- typ TTop t
        pure (at b (RuleBndrSig noAnn nm (HsPS noAnn ty)))
      _ -> at b . RuleBndr noAnn <$> varName' b

-------------------------------------------------------------------------------
-- Bindings

bindDecl :: DeclCtx -> Form -> [Form] -> P (LHsDecl GhcPs)
bindDecl ctx f = \case
  (lhs : rhs) -> at f . ValD noExtField <$> binding ctx f lhs rhs
  [] -> failAt f "empty binding"

-- | A binding: function equation or pattern binding (SPEC §3.1).
binding :: DeclCtx -> Form -> Form -> [Form] -> P (HsBind GhcPs)
binding _ f lhs rhsForms
  | Just items@(_ : _ : _) <- kwForm "mod" lhs = do
      grhss <- rhsP f rhsForms
      mods <- mapM modifierP (init items)
      p <- pat PTop (last items)
      pure (PatBind noExtField p mods grhss)
binding _ f lhs rhsForms = do
  grhss <- rhsP f rhsForms
  case lhsShape lhs of
    LhsFun name fixity strict pats -> do
      nm <- varName' name
      ps' <- case fixity of
        Infix -> mapM lhsOperand (take 2 pats) >>= \ops -> (ops ++) <$> mapM (pat PArg) (drop 2 pats)
        Prefix -> mapM (pat PArg) pats
      let ctxt = FunRhs { mc_fun = nm, mc_fixity = fixity
                        , mc_strictness = if strict then SrcStrict else NoSrcStrict
                        , mc_an = noAnn }
          match = at f (Match noExtField ctxt (at lhs ps') grhss)
      pure (FunBind noExtField nm (mkMatchGroup FromSource noAnn (at f [match]) ))
    LhsPat -> do
      p <- pat PTop lhs
      pure (PatBind noExtField p [] grhss)

-- | An operand of an infix function lhs: an unparenthesized constructor
-- chain (@f :+ g <*> a :+ b@) or an ordinary operand.
lhsOperand :: Form -> P (LPat GhcPs)
lhsOperand x = case kwForm "lhs-chain" x of
  Just items | length items >= 3 -> patInfixChain x items
  Just [p] -> pat POperand p
  _ -> pat POperand x

data LhsShape
  = LhsFun Form LexicalFixity Bool [Form]   -- ^ name, fixity, strict, patterns
  | LhsPat

lhsShape :: Form -> LhsShape
lhsShape lhs = case lhs of
  _ | Just s <- symOf lhs, symKind s `elem` [VarId, VarSym] -> LhsFun lhs Prefix False []
  Form _ (FList [b, x])
    | isBangTok b, Just s <- symOf x, symKind s == VarId -> LhsFun x Prefix True []
  Form _ (FList (h : args))
    | Just s <- symOf h, symKind s == VarId, not (null args) -> LhsFun h Prefix False args
    | Just [x] <- kwForm "name" h, Just s <- symOf x, symKind s `elem` [VarId, VarSym], not (null args)
        -> LhsFun h Prefix False args
    | Just s <- symOf h, symKind s == VarSym, [_, _] <- args, not (isBangTok h && length args == 1)
        -> LhsFun h Infix False args
    | isKw "infix" h, [l, op, r] <- args, Just s <- symOf op, symKind s `elem` [VarId, VarSym]
        -> LhsFun op Infix False [l, r]
    | isKw "infix" h, odd (length args)
    , [(i, op)] <- [ (i, o) | (i, o) <- zip [0 :: Int ..] args, odd i, Just s <- [symOf o], symKind s `elem` [VarId, VarSym] ]
        -> let side xs = Form (formPsSpan lhs) (FList (Form (formPsSpan lhs) (FKeyword (fsLit "lhs-chain")) : xs))
           in LhsFun op Infix False [side (take i args), side (drop (i + 1) args)]
    | Form _ (FList (ih : iargs)) <- h, not (null args) -> case lhsShape (Form (formPsSpan h) (FList (ih : iargs))) of
        LhsFun n Infix st ps -> LhsFun n Infix st (ps ++ args)
        _ -> LhsPat
  _ -> LhsPat
  where
    isBangTok = isTok (\case ITbang -> True; ITvarsym s -> s == fsLit "!"; _ -> False)

-- | Right-hand side: an expression or guards, then an optional where.
rhsP :: Form -> [Form] -> P (GRHSs GhcPs (LHsExpr GhcPs))
rhsP f forms = do
  let (body, whereF) = case reverse forms of
        (w : r) | Just ws <- tokForm (\case ITwhere -> True; _ -> False) w -> (reverse r, Just ws)
        _ -> (forms, Nothing)
  binds <- maybe (pure (EmptyLocalBinds noExtField)) localBindsP whereF
  grhss <- grhsList f expr body
  pure (GRHSs emptyComments grhss binds)

grhsList :: LBody body => Form -> (EPos -> Form -> P (LocatedA (body GhcPs)))
         -> [Form] -> P (NonEmpty (LGRHS GhcPs (LocatedA (body GhcPs))))
grhsList f bodyP = \case
  [e] | Nothing <- tokForm isVbar e -> do
    b <- bodyP ETop e
    pure (at e (GRHS noAnn [] b) :| [])
  gs@(_ : _) | all (isJust . tokForm isVbar) gs -> do
    rs <- mapM (guardedRhs bodyP) gs
    pure (NE.fromList rs)
  _ -> failAt f "expected a right-hand side"
  where
    isVbar = \case ITvbar -> True; _ -> False

guardedRhs :: LBody body => (EPos -> Form -> P (LocatedA (body GhcPs))) -> Form -> P (LGRHS GhcPs (LocatedA (body GhcPs)))
guardedRhs bodyP g = case tokForm (\case ITvbar -> True; _ -> False) g of
  Just items@(_ : _) -> do
    quals <- mapM stmtP (init items)
    b <- bodyP ETop (last items)
    pure (at g (GRHS noAnn quals b))
  _ -> failAt g "expected (| guard... rhs)"

localBindsP :: [Form] -> P (HsLocalBinds GhcPs)
localBindsP [] = pure (HsValBinds noAnn (ValBinds noExtField []))
localBindsP forms
  | all isIPBind forms = do
      bs <- forM forms $ \b -> case tokForm (\case ITequal -> True; _ -> False) b of
        Just [Form _ (FAtom (ITdupipvarid n)), e] -> do
          ex <- expr ETop e
          pure (at b (IPBind noAnn (at b (HsIPName n)) ex))
        _ -> failAt b "expected an implicit-parameter binding"
      pure (HsIPBinds noAnn (IPBinds noExtField bs))
  | otherwise = do
      ds <- concat <$> mapM (topDecl LocalCtx) forms
      vbs <- cvBindGroup (toOL ds)
      pure (HsValBinds noAnn vbs)
  where
    isIPBind b = case tokForm (\case ITequal -> True; _ -> False) b of
      Just (Form _ (FAtom (ITdupipvarid _)) : _) -> True
      _ -> False

sigDecl :: DeclCtx -> Form -> [Form] -> P (LHsDecl GhcPs)
sigDecl ctx f args = do
  (names, t) <- sigParts f args
  case ctx of
    _ | ctx `elem` [ClassCtx, InstCtx] -> pure (at f (SigD noExtField (ClassOpSig noAnn False names (hsTypeToHsSigType t))))
    _ -> pure (at f (SigD noExtField (TypeSig noAnn [] names (hsTypeToHsSigWcType t))))

sigParts :: Form -> [Form] -> P ([LocatedN RdrName], LHsType GhcPs)
sigParts f args = case args of
  (_ : _ : _) -> do
    names <- mapM varName' (init args)
    t <- typ TTop (last args)
    pure (names, t)
  _ -> failAt f "expected (:: name... type)"

fixityDecl :: FixityDirection -> Form -> [Form] -> P (LHsDecl GhcPs)
fixityDecl dir f args = do
  let (prec, src, r0) = case args of
        (Form _ (FAtom (ITinteger il)) : r) -> (fromIntegral (il_value il), il_text il, r)
        r -> (9, NoSourceText, r)
      (ns, r1) = case r0 of
        (h : r) | isTok (\case ITtype -> True; _ -> False) h -> (TypeNamespaceSpecifier noAnn, r)
                | isTok (\case ITdata -> True; _ -> False) h -> (DataNamespaceSpecifier noAnn, r)
        r -> (NoNamespaceSpecifier noExtField, r)
  names <- mapM (nameAt NSExpr) r1
  pure (at f (SigD noExtField (FixSig (noAnn, src) (FixitySig noExtField ns names (Fixity prec dir)))))

patSynDecl :: Form -> [Form] -> P (LHsDecl GhcPs)
patSynDecl f = \case
  [s] | Just (h : rest) <- listOf s, isDcolon h -> do
    names <- mapM (nameAt NSExpr) (init rest)
    t <- sigTypeP (last rest)
    pure (at f (SigD noExtField (PatSynSig noAnn names t)))
  (lhs : marker : def : rest) -> do
    (name, details) <- patSynLhs lhs
    p <- pat PTop def
    dir <- if
      | isEqual marker, null rest -> pure ImplicitBidirectional
      | isLArrow marker, null rest -> pure Unidirectional
      | isLArrow marker, [w] <- rest, Just eqs <- tokForm (\case ITwhere -> True; _ -> False) w -> do
          ds <- concat <$> mapM (topDecl LocalCtx) eqs
          mg <- mkPatSynMatchGroup name (at w (toOL ds, noAnn, noAnn))
          pure (ExplicitBidirectional mg)
      | otherwise -> failAt f "expected (pattern lhs = pat) or (pattern lhs <- pat)"
    pure (at f (ValD noExtField (PatSynBind noExtField (PSB noAnn name details p dir))))
  _ -> failAt f "malformed pattern synonym"
  where
    patSynLhs lhs = case lhs of
      _ | isJust (symOf lhs) -> do { n <- nameAt NSExpr lhs; pure (n, PrefixCon noExtField []) }
      Form _ (FList (h : args))
        | isKw "infix" h, [l, op, r] <- args -> do
            n <- nameAt NSExpr op
            a <- varName' l
            b <- varName' r
            pure (n, InfixCon noExtField a b)
        | isKw "rec" h, (c : fs) <- args -> do
            n <- nameAt NSExpr c
            flds <- forM fs $ \x -> do
              v <- varName' x
              pure (RecordPatSynField (FieldOcc noExtField v) v)
            pure (n, RecCon noAnn flds)
        | Just s <- symOf h, symKind s == ConSym, [l, r] <- args -> do
            n <- nameAt NSExpr h
            a <- varName' l
            b <- varName' r
            pure (n, InfixCon noExtField a b)
        | otherwise -> do
            n <- nameAt NSExpr h
            as <- mapM varName' args
            pure (n, PrefixCon noExtField as)
      _ -> failAt lhs "malformed pattern synonym head"

-------------------------------------------------------------------------------
-- Type-level declarations

-- | A declaration head with optional context: @(=> ctx... head)@.
headWithCtx :: Form -> P (Maybe (LHsContext GhcPs), Form)
headWithCtx f = case tokForm (\case ITdarrow _ -> True; _ -> False) f of
  Just args@(_ : _) -> do
    cs <- mapM (typ TCtxElem) (init args)
    pure (Just (ctxAt f (init args) (HsContext noAnn cs)), last args)
  _ -> pure (Nothing, f)

-- | A head type: @(:: head K)@ splits into the head and a kind signature.
headWithKind :: Form -> P (LHsType GhcPs, Maybe (LHsKind GhcPs))
headWithKind f = case tokForm (\case ITdcolon _ -> True; _ -> False) f of
  Just [h, k] -> do
    t <- typ TTop h
    kd <- typ TTop k
    pure (t, Just kd)
  _ -> (, Nothing) <$> typ TTop f

dataDecl :: DeclCtx -> Form -> NewOrData -> Bool -> [Form] -> P (LHsDecl GhcPs)
dataDecl ctx f nd isTypeData (fam : rest)
  | isName "family" fam || isTok (\case ITfamily -> True; _ -> False) fam =
      familyDecl ctx f DataFamily rest
dataDecl ClassCtx f DataType False args = familyDecl ClassCtx f DataFamily args
dataDecl ctx f nd isTypeData args0 = do
  let (ctype, args1) = case args0 of
        (c : r) | isJust (kwForm "ctype" c) -> (Just c, r)
        r -> (Nothing, r)
  case args1 of
    [] -> failAt f "a data declaration needs a head"
    (hd : rest) -> do
      -- family instances: (data instance head ...)
      (isInst, hd', rest') <- case (hd, rest) of
        (i, h : r) | isTok (\case ITinstance -> True; _ -> False) i -> pure (True, h, r)
        _ -> pure (False, hd, rest)
      let isFamInst = isInst || ctx == InstCtx
      ct <- traverse ctypeP ctype
      (mctx, hdF) <- headWithCtx hd'
      (bndrs, hdF') <- outerBndrs hdF
      (headTy, kind) <- headWithKind hdF'
      let (consForms, derivForms) = span (not . isDeriving) rest'
      cons <- consP consForms
      derivs <- mapM derivClauseP derivForms
      let sp = formSpan f
      if isFamInst
        then do
          L l d <- mkDataFamInst sp nd ct (mctx, bndrs, headTy) kind cons (L sp derivs) noAnn
          pure (L l (InstD noExtField d))
        else do
          L l d <- mkTyData sp isTypeData nd ct (L sp (mctx, headTy)) kind cons (L sp derivs) noAnn
          pure (L l (TyClD noExtField d))
  where
    isDeriving x = isJust (tokForm (\case ITderiving -> True; _ -> False) x)
    ctypeExt ns = CTypeGhc NoSourceText ns noAnn
    ctypeP c = case kwForm "ctype" c of
      Just [Form _ (FAtom (ITstring hs _ h)), Form _ (FAtom (ITstring ns _ n))] ->
        pure (at c (CType (ctypeExt ns) (Just (Header hs h)) n))
      Just [Form _ (FAtom (ITstring ns _ n))] -> pure (at c (CType (ctypeExt ns) Nothing n))
      _ -> failAt c "malformed :ctype"

-- | An explicit forall on a family-instance head: @(forall tv... head)@.
outerBndrs :: Form -> P (HsOuterFamEqnTyVarBndrs GhcPs, Form)
outerBndrs f = case tokForm (\case ITforall _ -> True; _ -> False) f of
  Just args@(_ : _) -> do
    bs <- mapM (tyVarBndrP (const (pure ()))) (init args)
    pure (mkHsOuterExplicit noAnn bs, last args)
  _ -> pure (mkHsOuterImplicit, f)

consP :: [Form] -> P [LConDecl GhcPs]
consP = \case
  [w] | Just cs <- tokForm (\case ITwhere -> True; _ -> False) w -> concat <$> mapM gadtCon cs
  cs -> mapM h98Con cs

gadtCon :: Form -> P [LConDecl GhcPs]
gadtCon f = case listOf f of
  Just (h : rest) | isDcolon h, length rest >= 2 -> do
    names <- mapM (nameAt NSExpr) (init rest)
    t <- gadtType (last rest)
    c <- mkGadtDecl (formSpan f) [] (NE.fromList names) noAnn (hsTypeToHsSigType t)
    pure [c]
  Just (h : rest) | isKw "mod" h, not (null rest) -> do
    mods <- mapM modifierP (init rest)
    cs <- gadtCon (last rest)
    pure [ L l c { con_modifiers = mods } | L l c <- cs ]
  _ -> failAt f "expected (:: Con... type) in a GADT"

-- | A GADT constructor type: record arguments are @(:record (:: f T)...)@.
gadtType :: Form -> P (LHsType GhcPs)
gadtType f = typ TTop f

h98Con :: Form -> P (LConDecl GhcPs)
h98Con f = case f of
  _ | isNameForm f -> do
    n <- nameAt NSExpr f
    pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (PrefixCon noExtField [])))
  Form _ (FList [h, c]) | isKw "rec" h -> do
    n <- nameAt NSExpr c
    pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (RecCon noAnn (at f []))))
  Form _ (FList (h : args))
    | isTok (\case ITforall _ -> True; _ -> False) h, not (null args) -> do
        tvs <- mapM (tyVarBndrP specP) (init args)
        L _ c <- h98Con (last args)
        pure (at f c { con_forall = True, con_ex_tvs = tvs })
    | isTok (\case ITdarrow _ -> True; _ -> False) h, not (null args) -> do
        cs <- mapM (typ TCtxElem) (init args)
        L _ c <- h98Con (last args)
        pure (at f c { con_mb_cxt = Just (ctxAt h (init args) (HsContext noAnn cs)) })
    | isKw "mod" h, not (null args) -> do
        mods <- mapM modifierP (init args)
        L _ c <- h98Con (last args)
        pure (at f c { con_modifiers = mods })
    | isKw "infix" h, [l, op, r] <- args -> do
        n <- nameAt NSExpr op
        a <- conField TOperand l
        b <- conField TOperand r
        pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (InfixCon noExtField a b)))
    | Just s <- symOf h, symKind s == ConSym, [l, r] <- args -> do
        n <- nameAt NSExpr h
        a <- conField TOperand l
        b <- conField TOperand r
        pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (InfixCon noExtField a b)))
    | all isRecGroup args, not (null args) -> do
        n <- nameAt NSExpr h
        flds <- mapM recFieldP args
        pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (RecCon noAnn (at f flds))))
    | otherwise -> do
        n <- nameAt NSExpr h
        fs <- mapM (conField TArg) args
        pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (PrefixCon noExtField fs)))
  _ -> do
    n <- nameAt NSExpr f
    pure (at f (mkConDeclH98 noAnn [] n Nothing Nothing (PrefixCon noExtField [])))
  where
    isRecGroup x = isJust (tokForm (\case ITdcolon _ -> True; _ -> False) x)

recFieldP :: Form -> P (LHsConDeclRecField GhcPs)
recFieldP f = case tokForm (\case ITdcolon _ -> True; _ -> False) f of
  Just args@(_ : _ : _) -> do
    names <- mapM (\n -> at n . FieldOcc noExtField <$> varName' n) (init args)
    let tyF = last args
    (mods, tyF') <- case kwForm "mod" tyF of
      Just ms@(_ : _) -> (, last ms) <$> mapM modifierP (init ms)
      _ -> pure ([], tyF)
    cdf <- conField TTop tyF'
    let cdf' = cdf { cdf_multiplicity = HsModifiedFunArr noExtField mods (HsStandardArr (EpColon noAnn)) }
    pure (at f (HsConDeclRecField noExtField names cdf'))
  _ -> failAt f "expected (:: field... type)"

-- | A constructor field with strictness and unpackedness wrappers.
conField :: TPos -> Form -> P (HsConDeclField GhcPs)
conField pos f = case f of
  Form _ (FList [h, x])
    | isKw "unpack" h -> setUnpack SrcUnpack <$> conField pos x
    | isKw "nounpack" h -> setUnpack SrcNoUnpack <$> conField pos x
    | isTok (\case ITbang -> True; ITvarsym s -> s == fsLit "!"; _ -> False) h ->
        setBang SrcStrict <$> conField TArg x
    | isTok (\case ITtilde -> True; ITvarsym s -> s == fsLit "~"; _ -> False) h ->
        setBang SrcLazy <$> conField TArg x
  _ -> hsPlainTypeField <$> typ pos f
  where
    setUnpack u c = c { cdf_unpack = u, cdf_ext = (noAnn, SourceText (fsLit (if u == SrcUnpack then "{-# UNPACK" else "{-# NOUNPACK"))) }
    setBang b c = c { cdf_bang = b }

derivClauseP :: Form -> P (LHsDerivingClause GhcPs)
derivClauseP f = do
  let args = fromMaybe [] (tokForm (\case ITderiving -> True; _ -> False) f)
  (strat, rest) <- case args of
    (s : r) | isName "stock" s || isTok (\case ITstock -> True; _ -> False) s -> pure (Just (at s (StockStrategy noAnn)), r)
            | isName "anyclass" s || isTok (\case ITanyclass -> True; _ -> False) s -> pure (Just (at s (AnyclassStrategy noAnn)), r)
            | isTok (\case ITnewtype -> True; _ -> False) s -> pure (Just (at s (NewtypeStrategy noAnn)), r)
    r -> pure (Nothing, r)
  case rest of
    [tys] -> mk strat tys
    [tys, v, t] | isName "via" v || isTok (\case ITvia -> True; _ -> False) v -> do
      vt <- hsTypeToHsSigType <$> typ TParen t
      mk (Just (at v (ViaStrategy (XViaStrategyPs noAnn vt)))) tys
    _ -> failAt f "malformed deriving clause"
  where
    mk strat tys = case tys of
      Form _ (FVector ts) -> do
        sts <- mapM sigTypeP ts
        pure (at f (HsDerivingClause noAnn strat (at tys (DctMulti noAnn sts))))
      _ -> do
        st <- sigTypeP tys
        pure (at f (HsDerivingClause noAnn strat (at tys (DctSingle noExtField st))))

typeDecl :: DeclCtx -> Form -> [Form] -> P [LHsDecl GhcPs]
typeDecl ctx f args = case args of
  -- type family / data family
  (fam : rest) | isName "family" fam || isTok (\case ITfamily -> True; _ -> False) fam ->
    (: []) <$> familyDecl ctx f OpenTypeFamily rest
  (d : rest) | isTok (\case ITdata -> True; _ -> False) d ->
    (: []) <$> dataDecl ctx f DataType True rest
  (i : [eqn]) | isTok (\case ITinstance -> True; _ -> False) i -> do
    e <- famEqnP eqn
    L l d <- mkTyFamInst (formSpan f) (unLoc e) noAnn noAnn
    pure [L l (InstD noExtField d)]
  (r : t : roles) | isName "role" r || isTok (\case ITrole -> True; _ -> False) r -> do
    n <- nameAt NSType t
    L l d <- mkRoleAnnotDecl (formSpan f) n [ L (formSpan x) (roleOf x) | x <- roles ] noAnn
    pure [L l (RoleAnnotD noExtField d)]
  [e] | ctx `elem` [ClassCtx, InstCtx], Just [_, _] <- tokForm (\case ITequal -> True; _ -> False) e -> do
    eq <- famEqnP e
    L l d <- mkTyFamInst (formSpan f) (unLoc eq) noAnn noAnn
    pure [L l (InstD noExtField d)]
  [e] | ctx `elem` [ClassCtx, InstCtx], Just (h : _) <- listOf e, isTok (\case ITforall _ -> True; _ -> False) h -> do
    eq <- famEqnP e
    L l d <- mkTyFamInst (formSpan f) (unLoc eq) noAnn noAnn
    pure [L l (InstD noExtField d)]
  [hd] | ctx == ClassCtx -> (: []) <$> familyDecl ctx f OpenTypeFamily [hd]
  (hd : r : rest) | ctx == ClassCtx, isFamilyResult r -> (: []) <$> familyDecl ctx f OpenTypeFamily (hd : r : rest)
  [s] | Just [n, k] <- tokForm (\case ITdcolon _ -> True; _ -> False) s -> do
    nm <- nameAt NSType n
    -- the kind may itself be a kind signature: type T :: a :: K
    kd <- hsTypeToHsSigType <$> typ TParen k
    pure [at f (KindSigD noExtField (StandaloneKindSig noAnn nm kd))]
  [hd, rhs]
    | ctx == ClassCtx -> do
        -- associated type default: type F a = rhs
        e <- famEqnP (Form (formPsSpan f) (FList [Form (formPsSpan f) (FAtom ITequal), hd, rhs]))
        L l d <- mkTyFamInst (formSpan f) (unLoc e) noAnn noAnn
        pure [L l (InstD noExtField d)]
    | ctx == InstCtx -> do
        e <- famEqnP (Form (formPsSpan f) (FList [Form (formPsSpan f) (FAtom ITequal), hd, rhs]))
        L l d <- mkTyFamInst (formSpan f) (unLoc e) noAnn noAnn
        pure [L l (InstD noExtField d)]
    | otherwise -> do
        lhs <- typ TTop hd
        r <- typ TParen rhs
        L l d <- mkTySynonym (formSpan f) lhs r noAnn noAnn
        pure [L l (TyClD noExtField d)]
  _ -> failAt f "malformed type declaration"
  where
    roleOf x = if isUnderscore x then Nothing else plainName x
    isFamilyResult r = case tokForm (\case ITequal -> True; ITvbar -> True; _ -> False) r of
      Just [_] -> True
      Just (_ : _ : _) | isJust (tokForm (\case ITvbar -> True; _ -> False) r) -> True
      _ -> False

familyDecl :: DeclCtx -> Form -> FamilyInfo GhcPs -> [Form] -> P (LHsDecl GhcPs)
familyDecl ctx f info0 args = case args of
  (hd : rest) -> do
    (headTy, kind) <- headWithKind hd
    let (resF, rest1) = case rest of
          (r : more) | Just [_] <- tokForm (\case ITequal -> True; _ -> False) r -> (Just r, more)
          more -> (Nothing, more)
        (injF, rest2) = case rest1 of
          (i : more) | isJust (tokForm (\case ITvbar -> True; _ -> False) i) -> (Just i, more)
          more -> (Nothing, more)
    res <- case (resF, kind) of
      (Just r, _) | Just [b] <- tokForm (\case ITequal -> True; _ -> False) r ->
        at r . TyVarSig noExtField <$> tyVarBndrP (const (pure ())) b
      (_, Just k) -> pure (L noSrcSpanA (KindSig noExtField k))
      _ -> pure (L noSrcSpanA (NoSig noExtField))
    inj <- case injF of
      Just i | Just (l : arr : rs) <- tokForm (\case ITvbar -> True; _ -> False) i, isRArrow arr -> do
        ln <- nameAt NSType l
        rns <- mapM (nameAt NSType) rs
        pure (Just (at i (InjectivityAnn noAnn ln rns)))
      Just i -> failAt i "malformed injectivity annotation"
      Nothing -> pure Nothing
    info <- case rest2 of
      [] -> pure info0
      [w] | Just eqs <- tokForm (\case ITwhere -> True; _ -> False) w -> case eqs of
        [d] | isDotDot d -> pure (ClosedTypeFamily Nothing)
        _ -> ClosedTypeFamily . Just <$> mapM famEqnP eqs
      (x : _) -> failAt x "unexpected form in a family declaration"
    let top = if ctx == ClassCtx then NotTopLevel else TopLevel
    L l d <- mkFamDecl (formSpan f) info top headTy res inj noAnn
    pure (L l (TyClD noExtField d))
  [] -> failAt f "a family declaration needs a head"

famEqnP :: Form -> P (LTyFamInstEqn GhcPs)
famEqnP f = do
  (bndrs, eqF) <- outerBndrs f
  case tokForm (\case ITequal -> True; _ -> False) eqF of
    Just [l, r] -> do
      lhs <- typ TTop l
      -- the right-hand side may be a kind signature: F Int = Any :: j -> j
      rhs <- typ TParen r
      mkTyFamInstEqn (formSpan f) bndrs lhs rhs noAnn
    _ -> failAt f "expected (= lhs rhs)"

classDecl :: Form -> [Form] -> P (LHsDecl GhcPs)
classDecl f = \case
  (hd : rest) -> do
    (mctx, hdF) <- headWithCtx hd
    headTy <- typ TTop hdF
    let (fdF, body) = case rest of
          (x : r) | Just _ <- tokForm (\case ITvbar -> True; _ -> False) x -> (Just x, r)
          r -> (Nothing, r)
    fds <- case fdF of
      Nothing -> pure []
      Just x -> forM (fromMaybe [] (tokForm (\case ITvbar -> True; _ -> False) x)) $ \fd ->
        case listOf fd of
          Just items | (l, _ : r) <- break isRArrow items -> do
            ls <- mapM (nameAt NSType) l
            rs <- mapM (nameAt NSType) r
            pure (at fd (FunDep noAnn ls rs))
          _ -> failAt fd "expected (a b -> c)"
    ds <- concat <$> mapM (topDecl ClassCtx) body
    L l d <- mkClassDecl (formSpan f) (L (formSpan hd) (mctx, headTy)) (L (formSpan f) ((), fds))
               (toOL ds) EpNoLayout noAnn
    pure (L l (TyClD noExtField d))
  [] -> failAt f "a class declaration needs a head"

instanceDecl :: Form -> [Form] -> P (LHsDecl GhcPs)
instanceDecl f args0 = do
  let (warnF, args1) = case args0 of
        (w : r) | isJust (kwForm "deprecated" w) || isJust (kwForm "warning" w) -> (Just w, r)
        r -> (Nothing, r)
      (ov, args2) = overlapP args1
  warn <- traverse warningTxtP warnF
  case args2 of
    (t : body) -> do
      ty <- sigTypeP t
      ds <- concat <$> mapM (topDecl InstCtx) body
      binds <- cvBindsAndSigs (toOL ds)
      pure (at f (InstD noExtField (ClsInstD noExtField
        (ClsInstDecl (warn, noAnn) [] ty binds (fmap (at f) ov)))))
    [] -> failAt f "an instance needs a head"

overlapP :: [Form] -> (Maybe (OverlapMode GhcPs), [Form])
overlapP = \case
  (Form _ (FKeyword k) : r)
    | k == fsLit "overlapping" -> (Just (Overlapping src), r)
    | k == fsLit "overlappable" -> (Just (Overlappable src), r)
    | k == fsLit "overlaps" -> (Just (Overlaps src), r)
    | k == fsLit "incoherent" -> (Just (Incoherent src), r)
    | k == fsLit "no-overlap" -> (Just (NoOverlap src), r)
    | k == fsLit "noncanonical" -> (Just (NonCanonical src), r)
  r -> (Nothing, r)
  where
    src = (NoSourceText, noAnn)

standaloneDeriving :: Form -> [Form] -> P (LHsDecl GhcPs)
standaloneDeriving f args0 = do
  (strat, args1) <- case args0 of
    (s : r) | isName "stock" s || isTok (\case ITstock -> True; _ -> False) s -> pure (Just (at s (StockStrategy noAnn)), r)
            | isName "anyclass" s || isTok (\case ITanyclass -> True; _ -> False) s -> pure (Just (at s (AnyclassStrategy noAnn)), r)
            | isTok (\case ITnewtype -> True; _ -> False) s -> pure (Just (at s (NewtypeStrategy noAnn)), r)
    (v : t : r) | isName "via" v || isTok (\case ITvia -> True; _ -> False) v -> do
      vt <- hsTypeToHsSigType <$> typ TParen t
      pure (Just (at v (ViaStrategy (XViaStrategyPs noAnn vt))), r)
    r -> pure (Nothing, r)
  case args1 of
    (i : rest) | isTok (\case ITinstance -> True; _ -> False) i -> do
      let (warnF, rest1) = case rest of
            (w : r) | isJust (kwForm "deprecated" w) || isJust (kwForm "warning" w) -> (Just w, r)
            r -> (Nothing, r)
          (ov, rest2) = overlapP rest1
      warn <- traverse warningTxtP warnF
      case rest2 of
        [t] -> do
          ty <- sigTypeP t
          pure (at f (DerivD noExtField (DerivDecl (warn, noAnn) (mkHsWildCardBndrs ty) strat (fmap (at f) ov))))
        _ -> failAt f "expected one instance type"
    _ -> failAt f "expected (deriving strategy? instance ...)"

defaultDecl :: [LHsModifier GhcPs] -> Form -> [Form] -> P (LHsDecl GhcPs)
defaultDecl mods f = \case
  [c, Form _ (FVector ts)] | Just s <- symOf c, symKind s == ConId -> do
    cls <- nameAt NSType c
    tys <- mapM (typ TParen) ts
    pure (at f (DefD noExtField (DefaultDecl noAnn mods (Just cls) tys)))
  ts -> do
    tys <- mapM (typ TParen) ts
    pure (at f (DefD noExtField (DefaultDecl noAnn mods Nothing tys)))

foreignDecl :: [LHsModifier GhcPs] -> Form -> [Form] -> P (LHsDecl GhcPs)
foreignDecl _ f = \case
  (kind : cc : rest) -> do
    conv <- case plainName cc of
      Just s | s == fsLit "ccall" -> pure CCallConv
             | s == fsLit "capi" -> pure CApiConv
             | s == fsLit "stdcall" -> pure StdCallConv
             | s == fsLit "prim" -> pure PrimCallConv
             | s == fsLit "javascript" -> pure JavaScriptCallConv
      _ -> failAt cc "expected a calling convention"
    let isImport = isTok (\case ITimport -> True; _ -> False) kind
        (safety, r1) = case rest of
          (s : r) | isImport, isName "safe" s -> (Just PlaySafe, r)
                  | isImport, isName "unsafe" s -> (Just PlayRisky, r)
                  | isImport, isName "interruptible" s -> (Just PlayInterruptible, r)
          r -> (Nothing, r)
    (entity, r2) <- case r1 of
      (Form sp (FAtom (ITstring st _ s)) : r) -> pure (L (mkSrcSpanPs sp) (StringLiteral st s), r)
      r -> pure (L (formSpan f) (StringLiteral NoSourceText (packHText "")), r)
    case r2 of
      [sigF] | Just [n, t] <- tokForm (\case ITdcolon _ -> True; _ -> False) sigF -> do
        nm <- varName' n
        ty <- sigTypeP t
        mk <- if isImport
          then mkImport (L (formSpan cc) conv) (L (formSpan f) (fromMaybe PlaySafe safety)) (entity, nm, ty) (noAnn, noAnn)
          else mkExport (L (formSpan cc) conv) (entity, nm, ty) (noAnn, noAnn)
        pure (at f (mk noAnn))
      _ -> failAt f "expected (:: name type) at the end of a foreign declaration"
  _ -> failAt f "malformed foreign declaration"

-------------------------------------------------------------------------------
-- Types

sigTypeP :: Form -> P (LHsSigType GhcPs)
sigTypeP f = hsTypeToHsSigType <$> typ TTop f

-- | Parse a type at a position, adding the implicit 'HsParTy' (SPEC I5).
typ :: TPos -> Form -> P (LHsType GhcPs)
typ pos f = do
  t <- typeForm f
  pure $ if typeNeedsParens pos (unLoc t)
    then at f (HsParTy noAnn t)
    else t

typeForm :: Form -> P (LHsType GhcPs)
typeForm f = case formNode f of
  FAtom t -> typeAtom f t
  FVector [] -> at f <$> mkListSyntaxTy0 noAnn noAnn (formSpan f)
  FVector [x] -> do
    t <- typ TTop x
    at f <$> mkListSyntaxTy1 noAnn t noAnn
  FVector xs -> at f . HsExplicitListTy noAnn NotPromoted <$> mapM (typ TTop) xs
  FList [] -> at f <$> mkTupleSyntaxTy noAnn [] noAnn
  FPrefix PTick x -> case formNode x of
    FVector xs -> at f . HsExplicitListTy noAnn IsPromoted <$> mapM (typ TTop) xs
    FList (h : xs) | isKw "tuple" h -> at f . HsExplicitTupleTy noAnn IsPromoted <$> mapM (typ TTop) xs
    _ | isJust (symOf x) || isNameForm x -> do
          n <- nameAt NSExpr x
          pure (at f (HsTyVar noAnn IsPromoted n))
    _ -> failAt f "unexpected promoted form"
  FPrefix _ _ -> failAt f "unexpected prefix in a type"
  FKeyword _ -> failAt f "unexpected keyword in a type"
  FList [h] | isKw "tuple" h -> at f <$> mkTupleSyntaxTy noAnn [] noAnn
            | isKw "utuple" h -> pure (at f (HsTupleTy noAnn HsUnboxedTuple []))
  FList [h] | isKw "record" h -> pure (at f (XHsType (HsRecTy noAnn (L (formSpan f) []))))
  FList [_] -> failAt f "a one-element list is not a type; write the type itself"
  FList _ | isNameForm f -> do
    n <- nameAt NSType f
    pure (at f (HsTyVar noAnn NotPromoted n))
  FList (h : args) -> case h of
    _ | isTok (\case ITforall _ -> True; _ -> False) h -> forallType f args
      | isTok (\case ITdarrow _ -> True; _ -> False) h -> do
          cs <- mapM (typ TCtxElem) (init args)
          body <- typ TTop (last args)
          pure (at f (HsQualTy noExtField (ctxAt h (init args) (HsContext noAnn cs)) body))
      | isRArrow h -> arrowChain f (HsStandardArr (EpArrow noAnn)) args
      | isTok (\case ITlolly -> True; _ -> False) h -> arrowChain f (HsLinearArr noAnn) args
      | isLinearArrowSym h -> arrowChain f (HsLinearArr noAnn) args
      | isDcolon h, [x, k] <- args -> case x of
          Form _ (FAtom (ITdupipvarid n)) -> at f . HsIParamTy noAnn (at x (HsIPName n)) <$> typ TTop k
          _ -> at f <$> (HsKindSig noAnn <$> typ TSigSubj x <*> typ TTop k)
      | isKw "tuple" h -> do
          ts <- mapM (typ TParen) args
          at f <$> mkTupleSyntaxTy noAnn ts noAnn
      | isKw "utuple" h -> at f . HsTupleTy noAnn HsUnboxedTuple <$> mapM (typ TParen) args
      | isKw "usum" h -> at f . HsSumTy noAnn <$> mapM (typ TParen) args
      | isKw "paren" h, [x] <- args -> at f . HsParTy noAnn <$> typ TParen x
      | isKw "record" h -> do
          flds <- mapM recFieldP args
          pure (at f (XHsType (HsRecTy noAnn (L (formSpan f) flds))))
      | isBangHead h, [x] <- args -> at f . mkBangTy noAnn SrcStrict <$> typ TArg x
      | isLazyHead h, [x] <- args -> at f . mkBangTy noAnn SrcLazy <$> typ TArg x
      | isKw "unpack" h, [x] <- args -> typ TArg x >>= addUnpackednessP (L (formSpan f) (UnpackednessPragma noAnn (SourceText (fsLit "{-# UNPACK")) SrcUnpack))
      | isKw "nounpack" h, [x] <- args -> typ TArg x >>= addUnpackednessP (L (formSpan f) (UnpackednessPragma noAnn (SourceText (fsLit "{-# NOUNPACK")) SrcNoUnpack))
      | isKw "infix" h -> typeInfixChain f args
      | isKw "splice" h || isKw "qq" h -> at f . HsSpliceTy noExtField <$> spliceForm f h args
      | isKw "tuple-con" h || isKw "utuple-con" h -> typeApp f h args
      | Just s <- symOf h, isSymbolic s, length args >= 2 -> typeOpChain f (Just h) args
      | FPrefix PTick op <- formNode h, Just s <- symOf op, isSymbolic s, length args >= 2 -> typeOpChain f (Just h) args
      | otherwise -> typeApp f h args
  where
    isBangHead = isTok (\case ITbang -> True; ITvarsym s -> s == fsLit "!"; _ -> False)
    isLazyHead = isTok (\case ITtilde -> True; ITvarsym s -> s == fsLit "~"; _ -> False)
    isLinearArrowSym = isTok (\case ITvarsym s -> s == fsLit "->."; _ -> False)

typeAtom :: Form -> Token -> P (LHsType GhcPs)
typeAtom f t = case t of
  ITunderscore -> pure (at f (HsWildCardTy (HoleVar (at f unnamedHoleRdrName))))
  ITstar _ -> do
    star <- getBit StarIsTypeBit
    if star then pure (at f (HsStarTy noAnn)) else tyVar
  ITinteger il -> pure (at f (HsTyLit noExtField (HsNatural noExtField il)))
  ITrational fl -> pure (at f (HsTyLit noExtField (HsDouble noExtField fl)))
  ITstring st _ s -> pure (at f (HsTyLit noExtField (HsString st s)))
  ITchar st c -> pure (at f (HsTyLit noExtField (HsChar st c)))
  _ -> tyVar
  where
    tyVar = do
      n <- nameAt NSType f
      pure (at f (HsTyVar noAnn NotPromoted n))

forallType :: Form -> [Form] -> P (LHsType GhcPs)
forallType f args = do
  let (bndrForms, rest) = break isRArrow (init args)
  body <- typ TTop (last args)
  case rest of
    [] -> do
      bs <- mapM (tyVarBndrP specP) (init args)
      pure (at f (HsForAllTy noExtField (HsForAllInvis noAnn bs) body))
    [_] -> do
      bs <- mapM (tyVarBndrP (const (pure ()))) bndrForms
      pure (at f (HsForAllTy noExtField (HsForAllVis noAnn bs) body))
    _ -> failAt f "malformed forall"

specP :: Form -> P Specificity
specP x
  | isJust (kwForm "inferred" x) = pure InferredSpec
  | otherwise = pure SpecifiedSpec

-- | A type variable binder: @a@, @(:: a K)@, @(:inferred a)@, @\@a@, @_@.
tyVarBndrP :: forall flag. (Form -> P flag) -> Form -> P (LHsTyVarBndr flag GhcPs)
tyVarBndrP flagP f = go f
  where
    go x = case x of
      Form _ (FList [h, inner]) | isKw "inferred" h -> do
        L l b <- go inner
        fl <- flagP x
        pure (L l b { tvb_flag = fl })
      Form _ (FList [h, v, k]) | isDcolon h -> do
        var <- bndrVar v
        kd <- typ TTop k
        fl <- flagP x
        pure (at x (HsTvb noAnn fl var (HsBndrKind noExtField kd)))
      _ -> do
        var <- bndrVar x
        fl <- flagP x
        pure (at x (HsTvb noAnn fl var (HsBndrNoKind noExtField)))
    bndrVar v
      | isUnderscore v = pure (HsBndrWildCard (HoleVar (at v unnamedHoleRdrName)))
      | otherwise = HsBndrVar noExtField <$> nameAt NSType v

arrowChain :: Form -> HsFunArr GhcPs -> [Form] -> P (LHsType GhcPs)
arrowChain f arr args
  | length args < 2 = failAt f "an arrow needs an argument and a result"
  | otherwise = go args
  where
    go [r] = typ TTop r
    go (a : rest) = do
      (mods, aF) <- case kwForm "mod" a of
        Just ms@(_ : _) -> (, last ms) <$> mapM modifierP (init ms)
        _ -> pure ([], a)
      at' <- typ TArrowArg aF
      r <- go rest
      pure (atSpan (combineSrcSpans (formSpan a) (getLocA r))
              (HsFunTy noExtField (HsModifiedFunArr noExtField mods arr) at' r))
    go [] = failAt f "empty arrow"

typeApp :: Form -> Form -> [Form] -> P (LHsType GhcPs)
typeApp f h args = do
  hd <- case h of
    _ | isJust (kwForm "name" h) || isKw "tuple-con" h || isKw "utuple-con" h -> do
          n <- nameAt NSType h
          pure (at h (HsTyVar noAnn NotPromoted n))
      | otherwise -> typ TFun h
  foldM app hd args
  where
    app acc a = case a of
      Form _ (FPrefix PAt k) -> do
        kd <- typ TArg k
        pure (atSpan (combineSrcSpans (getLocA acc) (formSpan a)) (HsAppKindTy noAnn acc kd))
      _ -> do
        t <- typ TArg a
        pure (atSpan (combineSrcSpans (getLocA acc) (formSpan a)) (HsAppTy noExtField acc t))

-- | @(op a b c)@: the chain @a op b op c@, right-nested as GHC builds it.
typeOpChain :: Form -> Maybe Form -> [Form] -> P (LHsType GhcPs)
typeOpChain f (Just opF) args = do
  operands <- mapM (typ TOperand) args
  op <- typeOp opF
  pure (foldrList (\a r -> atSpan (combineSrcSpans (getLocA a) (getLocA r)) (HsOpTy noExtField a op r)) operands)
  where _ = f
typeOpChain f Nothing _ = failAt f "missing operator"

typeInfixChain :: Form -> [Form] -> P (LHsType GhcPs)
typeInfixChain f args
  | odd (length args), length args >= 3 = do
      let operandFs = [ x | (i, x) <- zip [0 :: Int ..] args, even i ]
          opFs = [ x | (i, x) <- zip [0 :: Int ..] args, odd i ]
      operands <- mapM (typ TOperand) operandFs
      ops <- mapM typeOp opFs
      let build [a] [] = a
          build (a : as) (o : os) = let r = build as os
                                    in atSpan (combineSrcSpans (getLocA a) (getLocA r)) (HsOpTy noExtField a o r)
          build _ _ = panicChain
      pure (build operands ops)
  | otherwise = failAt f "(:infix a op b ...) needs operands and operators alternating"
  where
    panicChain = error "typeInfixChain"

typeOp :: Form -> P (LHsType GhcPs)
typeOp f = case formNode f of
  FPrefix PTick x -> do
    n <- nameAt NSExpr x
    pure (at f (HsTyVar noAnn IsPromoted n))
  _ -> do
    n <- nameAt NSType f
    pure (at f (HsTyVar noAnn NotPromoted n))

-------------------------------------------------------------------------------
-- Expressions

-- | Parse an expression at a position, adding the implicit 'HsPar' (SPEC I5).
expr :: EPos -> Form -> P (LHsExpr GhcPs)
expr pos f = do
  e <- exprForm f
  po <- parenOptsP
  pure $ if exprNeedsParens po pos (unLoc e)
    then at f (HsPar noAnn e)
    else e

exprForm :: Form -> P (LHsExpr GhcPs)
exprForm f = case formNode f of
  FAtom t -> exprAtom f t
  FVector xs -> vectorExpr f xs
  FList [] -> pure (at f (HsVar noExtField (at f (getRdrName unitDataCon))))
  FPrefix PTick x -> at f . HsUntypedBracket noExtField . VarBr noAnn True <$> nameAt NSExpr x
  FPrefix PTyQuote x -> at f . HsUntypedBracket noExtField . VarBr noAnn False <$> nameAt NSType x
  FPrefix PAt _ -> failAt f "a type argument must follow a function"
  FKeyword _ -> failAt f "unexpected keyword in an expression"
  FList [h] | HTok (ITdo mm) <- headOf h -> doBlock f (DoExpr (fmap mkModuleNameFS mm)) []
            | HTok (ITmdo mm) <- headOf h -> doBlock f (MDoExpr (fmap mkModuleNameFS mm)) []
            | isTok (\case ITlcase -> True; _ -> False) h -> lamCase f LamCase []
            | isTok (\case ITlcases -> True; _ -> False) h -> lamCase f LamCases []
            | isKw "tuple" h -> pure (at f (ExplicitTuple noAnn [] Boxed))
            | isKw "utuple" h -> pure (at f (ExplicitTuple noAnn [] Unboxed))
  FList [h] | isKw "quote-decls" h -> keywordExpr f "quote-decls" []
  FList [_] -> failAt f "a one-element list is not an expression; write (:paren e) for parentheses"
  FList (h : args) -> case headOf h of
    HTok ITlam -> lambda f args
    HTok ITlcase -> lamCase f LamCase args
    HTok ITlcases -> lamCase f LamCases args
    HTok ITcase -> case args of
      (s : alts) -> do
        scrut <- expr ETop s
        ms <- mapM (altP expr) alts
        pure (at f (HsCase noAnn scrut (mkMatchGroup FromSource noAnn (at f ms))))
      [] -> failAt f "case needs a scrutinee"
    HTok ITif
      | all (isJust . tokForm (\case ITvbar -> True; _ -> False)) args, not (null args) -> do
          gs <- mapM (guardedRhs expr) args
          pure (at f (HsMultiIf noAnn (NE.fromList gs)))
      | [c, t, e] <- args -> do
          c' <- expr ETop c
          t' <- expr ETop t
          e' <- expr ETop e
          pure (at f (HsIf noAnn c' t' e'))
      | otherwise -> failAt f "expected (if c t e)"
    HTok ITlet -> case args of
      (_ : _) -> do
        binds <- localBindsP (init args)
        body <- expr ETop (last args)
        pure (at f (HsLet noAnn binds body))
      [] -> failAt f "let needs a body"
    HTok (ITdo mm) -> doBlock f (DoExpr (fmap mkModuleNameFS mm)) args
    HTok (ITmdo mm) -> doBlock f (MDoExpr (fmap mkModuleNameFS mm)) args
    HTok (ITdcolon _) | [e, t] <- args -> do
      e' <- expr ESigSubj e
      t' <- typ TTop t
      pure (at f (ExprWithTySig noAnn e' (hsTypeToHsSigWcType t')))
    HTok ITproc | [p, c] <- args -> do
      p' <- pat PArg p
      c' <- cmd c
      pure (at f (HsProc noAnn p' (at c (HsCmdTop noExtField (at c c')))))
    HTok ITstatic | [e] <- args -> at f . HsStatic noAnn <$> expr EAtom e
    HTok ITtype | [t] <- args -> at f . HsEmbTy noAnn . mkHsWildCardBndrs <$> typ TTop t
    HTok (ITforall _) -> do
      L _ t <- forallType f args
      case t of
        HsForAllTy _ tele _ -> at f . HsForAll noExtField tele <$> expr ETop (last args)
        _ -> failAt f "malformed forall"
    HTok (ITdarrow _) | not (null args) -> do
      cs <- mapM (expr ETop) (init args)
      body <- expr ETop (last args)
      pure (at f (HsQual noExtField (ctxAt h (init args) (HsContext noAnn cs)) body))
    HTok (ITrarrow _) | [a, b] <- args -> do
      -- type syntax in a term (RequiredTypeArguments): * is HsStar here
      let operand x
            | isTok (\case ITstar _ -> True; _ -> False) x = do
                star <- getBit StarIsTypeBit
                if star then pure (at x (HsStar noAnn)) else expr ETop x
            | otherwise = expr ETop x
      a' <- operand a
      b' <- operand b
      pure (at f (HsFunArr noExtField (HsModifiedFunArr noExtField [] (HsStandardArr (EpArrow noAnn))) a' b'))
    HKeyword k -> keywordExpr f (unpackFS k) args
    HSym s
      | symText s == fsLit "-", Nothing <- symMod s, [x] <- args -> do
          e <- expr ENegArg x
          pure (at f (NegApp noAnn e noSyntaxExpr))
      | isSymbolic s, length args >= 2 -> exprOpChain f h args
    _ -> appSpine f h args

exprAtom :: Form -> Token -> P (LHsExpr GhcPs)
exprAtom f t = case t of
  ITinteger il -> pure (at f (HsOverLit noExtField (mkHsIntegral il)))
  ITrational fl -> pure (at f (HsOverLit noExtField (mkHsFractional fl)))
  ITunderscore -> pure (at f (HsHole (HoleVar (at f unnamedHoleRdrName))))
  ITdupipvarid n -> pure (at f (HsIPVar noExtField (HsIPName n)))
  ITlabelvarid st l -> pure (at f (HsOverLabel st l))
  _ | Just l <- tokLit t -> pure (at f (HsLit noExtField l))
    | otherwise -> do
        n <- nameAt NSExpr f
        pure (at f (HsVar noExtField n))

-- | A literal token as an 'HsLit' (non-overloaded literals).
tokLit :: Token -> Maybe (HsLit GhcPs)
tokLit = \case
  ITchar st c -> Just (HsChar st c)
  ITstring st _ s -> Just (HsString st s)
  ITprimchar st c -> Just (HsCharPrim st c)
  ITprimstring st s -> Just (HsStringPrim st s)
  ITprimint st i -> Just (HsIntPrim st i)
  ITprimword st i -> Just (HsWordPrim st i)
  ITprimint8 st i -> Just (HsInt8Prim st i)
  ITprimint16 st i -> Just (HsInt16Prim st i)
  ITprimint32 st i -> Just (HsInt32Prim st i)
  ITprimint64 st i -> Just (HsInt64Prim st i)
  ITprimword8 st i -> Just (HsWord8Prim st i)
  ITprimword16 st i -> Just (HsWord16Prim st i)
  ITprimword32 st i -> Just (HsWord32Prim st i)
  ITprimword64 st i -> Just (HsWord64Prim st i)
  ITprimfloat fl -> Just (HsFloatPrim noExtField fl)
  ITprimdouble fl -> Just (HsDoublePrim noExtField fl)
  _ -> Nothing

vectorExpr :: Form -> [Form] -> P (LHsExpr GhcPs)
vectorExpr f xs = case xs of
  [] -> pure (at f (HsVar noExtField (at f (getRdrName nilDataCon))))
  [a, d] | isDotDot d -> seqE (From <$> e a)
  [a, b, d] | isDotDot d -> seqE (FromThen <$> e a <*> e b)
  [a, d, c] | isDotDot d -> seqE (FromTo <$> e a <*> e c)
  [a, b, d, c] | isDotDot d -> seqE (FromThenTo <$> e a <*> e b <*> e c)
  (body : bar : quals) | isBar bar -> do
    mc <- getBit MonadComprehensionsBit
    b <- e body
    groups <- mapM qualsP (splitOn isBar quals)
    stmts <- case groups of
      [g] -> pure g
      gs -> pure [at f (ParStmt noExtField (NE.fromList [ ParStmtBlock noExtField g [] noSyntaxExpr | g <- gs ]) noExpr noSyntaxExpr)]
    let lastS = at body (LastStmt noExtField b Nothing noSyntaxExpr)
    pure (at f (HsDo noAnn (if mc then MonadComp else ListComp) (at f (stmts ++ [lastS]))))
  _ -> at f . ExplicitList noAnn <$> mapM e xs
  where
    e = expr ETop
    seqE info = at f . ArithSeq noAnn Nothing <$> info
    noExpr = HsLit noExtField (HsString (SourceText (fsLit "noExpr")) (packHText "noExpr"))

-- | Comprehension qualifiers; a @then@ qualifier takes all the qualifiers
-- before it, as in GHC's grammar.
qualsP :: [Form] -> P [ExprLStmt GhcPs]
qualsP = go []
  where
    go acc [] = pure (reverse acc)
    go acc (q : qs)
      | Just args <- tokForm (\case ITthen -> True; _ -> False) q = do
          t <- transStmt q args (reverse acc)
          go [t] qs
      | otherwise = do
          s <- stmtP q
          go (s : acc) qs

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn p xs = case break p xs of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn p rest

lambda :: Form -> [Form] -> P (LHsExpr GhcPs)
lambda f args = case args of
  (_ : _ : _) -> do
    ps <- mapM (pat PArg) (init args)
    body <- expr ETop (last args)
    let m = at f (Match noExtField (LamAlt LamSingle) (at f ps)
                  (GRHSs emptyComments (at f (GRHS noAnn [] body) :| []) (EmptyLocalBinds noExtField)))
    pure (at f (HsLam noAnn LamSingle (mkMatchGroup FromSource noAnn (at f [m]))))
  _ -> failAt f "a lambda needs patterns and a body"

lamCase :: Form -> HsLamVariant -> [Form] -> P (LHsExpr GhcPs)
lamCase f variant alts = do
  ms <- mapM (altP' (LamAlt variant) expr) alts
  pure (at f (HsLam noAnn variant (mkMatchGroup FromSource noAnn (at f ms))))

-- | A case alternative: @(-> pat... rhs...)@.
altP :: LBody body => (EPos -> Form -> P (LocatedA (body GhcPs))) -> Form -> P (LMatch GhcPs (LocatedA (body GhcPs)))
altP = altP' CaseAlt

altP' :: LBody body => HsMatchContext (LIdP GhcPs) -> (EPos -> Form -> P (LocatedA (body GhcPs))) -> Form
      -> P (LMatch GhcPs (LocatedA (body GhcPs)))
altP' ctxt bodyP f = case tokForm (\case ITrarrow _ -> True; _ -> False) f of
  Just items@(_ : _) -> do
    let (body0, whereF) = case reverse items of
          (w : r) | Just ws <- tokForm (\case ITwhere -> True; _ -> False) w -> (reverse r, Just ws)
          _ -> (items, Nothing)
        isGuard x = isJust (tokForm (\case ITvbar -> True; _ -> False) x)
        (patFs, rhsFs) = case break isGuard body0 of
          (ps, gs@(_ : _)) -> (ps, gs)
          (ps, []) -> (init ps, [last ps])
    ps <- mapM (pat PTop) patFs
    binds <- maybe (pure (EmptyLocalBinds noExtField)) localBindsP whereF
    grhss <- grhsList f bodyP rhsFs
    pure (at f (Match noExtField ctxt (at f ps) (GRHSs emptyComments grhss binds)))
  _ -> failAt f "expected an alternative (-> pat rhs)"

doBlock :: Form -> HsDoFlavour -> [Form] -> P (LHsExpr GhcPs)
doBlock f flav stmts = do
  ss <- mapM stmtP stmts
  pure (at f (HsDo noAnn flav (at f ss)))

stmtP :: Form -> P (ExprLStmt GhcPs)
stmtP f = case f of
  Form _ (FList (h : args))
    | isLArrow h, [p, e] <- args -> do
        p' <- pat PTop p
        e' <- expr ETop e
        pure (at f (BindStmt noAnn p' e'))
    | isTok (\case ITlet -> True; _ -> False) h, all isBindingForm args -> do
        binds <- localBindsP args
        pure (at f (LetStmt noAnn binds))
    | isTok (\case ITrec -> True; _ -> False) h -> do
        ss <- mapM stmtP args
        pure (at f (RecStmt { recS_ext = noAnn, recS_stmts = at f ss, recS_later_ids = []
                            , recS_rec_ids = [], recS_bind_fn = noSyntaxExpr
                            , recS_ret_fn = noSyntaxExpr, recS_mfix_fn = noSyntaxExpr }))
    | isTok (\case ITthen -> True; _ -> False) h -> transStmt f args []
  _ -> do
    e <- expr ETop f
    pure (at f (BodyStmt noExtField e noSyntaxExpr noSyntaxExpr))
  where
    isBindingForm x = case x of
      Form _ (FList (b : _)) -> isEqual b || isDcolon b
                                || isTok (\case ITinfixl -> True; ITinfixr -> True; ITinfix -> True; _ -> False) b
                                || (case b of Form _ (FKeyword _) -> True; _ -> False)
      _ -> False

transStmt :: Form -> [Form] -> [ExprLStmt GhcPs] -> P (ExprLStmt GhcPs)
transStmt f args prev = case args of
  (g : rest) | isName "group" g || isTok (\case ITgroup -> True; _ -> False) g -> do
    let (byF, rest') = case rest of
          (b : e : r) | isName "by" b || isTok (\case ITby -> True; _ -> False) b -> (Just e, r)
          r -> (Nothing, r)
    case rest' of
      [u, e] | isName "using" u || isTok (\case ITusing -> True; _ -> False) u -> do
        using <- expr ETop e
        by <- traverse (expr ETop) byF
        pure (at f (mkTrans GroupForm using by))
      _ -> failAt f "expected (then group by? e using f)"
  (e : rest) -> do
    using <- expr ETop e
    by <- case rest of
      [b, x] | isName "by" b || isTok (\case ITby -> True; _ -> False) b -> Just <$> expr ETop x
      [] -> pure Nothing
      _ -> failAt f "expected (then f by? e)"
    pure (at f (mkTrans ThenForm using by))
  [] -> failAt f "malformed then"
  where
    mkTrans form using by = TransStmt
      { trS_ext = noAnn, trS_form = form, trS_stmts = prev, trS_bndrs = []
      , trS_using = using, trS_by = by, trS_ret = noSyntaxExpr
      , trS_bind = noSyntaxExpr, trS_fmap = HsLit noExtField (HsString (SourceText (fsLit "noExpr")) (packHText "noExpr")) }

keywordExpr :: Form -> String -> [Form] -> P (LHsExpr GhcPs)
keywordExpr f k args = case k of
  "paren" | [x] <- args -> at f . HsPar noAnn <$> expr EParen x
  "tuple" -> tuple Boxed
  "utuple" -> tuple Unboxed
  "usum" -> do
    let width = length args
        holes = map isHole args
    case [ (i, x) | (i, x) <- zip [1 ..] args, not (isHole x) ] of
      [(tag, x)] | length (filter id holes) == width - 1 ->
        at f . ExplicitSum noAnn tag width <$> expr ETop x
      _ -> failAt f "(:usum ...) needs exactly one non-:_ element"
  "infix" -> exprInfixChain f args
  "section-l" | [e, op] <- args -> do
    e' <- expr ESectionL e
    o <- opExpr op
    pure (at f (SectionL noExtField e' o))
  "section-r" | [op, e] <- args -> do
    o <- opExpr op
    e' <- expr ESectionR e
    pure (at f (SectionR noExtField o e'))
  "rec" | (c : flds) <- args -> do
    con <- nameAt NSExpr c
    fs <- recFieldsP' (expr ETop) punPlaceholder flds
    pure (at f (RecordCon noExtField con fs))
  "update" | (e : flds) <- args -> do
    e' <- expr EAtom e
    upd <- recUpdP flds
    pure (at f (RecordUpd noAnn e' upd))
  "get" | (e : fs) <- args, not (null fs) -> do
    e' <- expr EAtom e
    foldM (\acc x -> do
             lbl <- fieldLabel x
             pure (atSpan (combineSrcSpans (getLocA acc) (formSpan x))
                     (HsGetField noExtField acc (at x (DotFieldOcc noAnn (at x lbl))))))
          e' fs
  "proj" | not (null args) -> do
    lbls <- mapM (\x -> do { l <- fieldLabel x; pure (DotFieldOcc noAnn (at x l)) }) args
    pure (at f (HsProjection noAnn (NE.fromList lbls)))
  "quote" | [e] <- args -> at f . HsUntypedBracket noExtField . ExpBr noAnn <$> expr ETop e
  "quote-pat" | [p] <- args -> at f . HsUntypedBracket noExtField . PatBr noAnn <$> pat PTop p
  "quote-type" | [t] <- args -> at f . HsUntypedBracket noExtField . TypBr noAnn <$> typ TParen t
  "quote-decls" -> do
    ds <- concat <$> mapM (topDecl TopCtx) args
    pure (at f (HsUntypedBracket noExtField (DecBrL noAnn (cvTopDecls (toOL ds)))))
  "typed-quote" | [e] <- args -> at f . HsTypedBracket noAnn <$> expr ETop e
  "splice" | [e] <- args -> at f . HsUntypedSplice noExtField <$> untypedSpliceExpr f e
  "qq" -> at f . HsUntypedSplice noExtField <$> quasiQuote f args
  "typed-splice" | [e] <- args -> at f . HsTypedSplice noExtField . HsTypedSpliceExpr noAnn <$> expr EAtom e
  "scc" | [lbl, e] <- args -> do
    sl <- case lbl of
      Form _ (FAtom (ITstring st _ s)) -> pure (StringLiteral st s)
      _ | Just s <- plainName lbl -> pure (StringLiteral NoSourceText (packHText (unpackFS s)))
      _ -> failAt lbl "expected a label"
    e' <- expr ETop e
    pure (at f (HsPragE noExtField (HsPragSCC (noAnn, NoSourceText) sl) e'))
  "qual-lit" | [m, s] <- args -> at f . HsQualLit noExtField <$> qualLitP m s
  "name" -> nameExpr
  "tuple-con" -> nameExpr
  "usum-con" -> nameExpr
  "utuple-con" -> nameExpr
  _ -> failAt f ("unknown expression form :" ++ k)
  where
    nameExpr = at f . HsVar noExtField <$> nameAt NSExpr f
    tuple boxity = do
      as <- forM args $ \x ->
        if isHole x then pure (Missing noAnn)
        else Present noExtField <$> expr ETop x
      pure (at f (ExplicitTuple noAnn as boxity))

qualLitP :: Form -> Form -> P (HsQualLit GhcPs)
qualLitP m s = do
  L _ mn <- moduleName m
  case s of
    Form _ (FAtom (ITstring st _ str)) -> pure (QualLit noExtField mn (HsQualString st str))
    _ -> failAt s "expected a string"

fieldLabel :: Form -> P FieldLabelString
fieldLabel x = case plainName x of
  Just s -> pure (FieldLabelString (packHText (unpackFS s)))
  Nothing -> failAt x "expected a field name"

untypedSpliceExpr :: Form -> Form -> P (HsUntypedSplice GhcPs)
untypedSpliceExpr _ e = HsUntypedSpliceExpr noAnn <$> expr EAtom e

quasiQuote :: Form -> [Form] -> P (HsUntypedSplice GhcPs)
quasiQuote f = \case
  [q, s@(Form _ (FAtom (ITstring _ _ txt)))] -> do
    qn <- varName' q
    pure (HsQuasiQuote noExtField qn (at s txt))
  _ -> failAt f "expected (:qq quoter \"text\")"

spliceForm :: Form -> Form -> [Form] -> P (HsUntypedSplice GhcPs)
spliceForm f h args
  | isKw "splice" h, [e] <- args = untypedSpliceExpr f e
  | otherwise = quasiQuote f args

opExpr :: Form -> P (LHsExpr GhcPs)
opExpr op
  | isUnderscore op = pure (at op (HsHole (HoleVar (at op unnamedHoleRdrName))))
  | otherwise = do
      n <- nameAt NSExpr op
      pure (at op (HsVar noExtField n))

-- | @(op a b c)@: the chain @a op b op c@, left-nested, unresolved.
exprOpChain :: Form -> Form -> [Form] -> P (LHsExpr GhcPs)
exprOpChain _ opF args = do
  let n = length args
  operands <- zipWithM (\i x -> expr (posOf n i) x) [1 ..] args
  op <- opExpr opF
  pure (foldlList (\l r -> atSpan (combineSrcSpans (getLocA l) (getLocA r)) (OpApp noExtField l op r)) operands)

posOf :: Int -> Int -> EPos
posOf n i | i == 1 = EOpFirst
          | i == n = EOpLast
          | otherwise = EOpMid

exprInfixChain :: Form -> [Form] -> P (LHsExpr GhcPs)
exprInfixChain f args
  | odd (length args), length args >= 3 = do
      let operandFs = [ x | (i, x) <- zip [0 :: Int ..] args, even i ]
          opFs = [ x | (i, x) <- zip [0 :: Int ..] args, odd i ]
          n = length operandFs
      operands <- zipWithM (\i x -> expr (posOf n i) x) [1 ..] operandFs
      ops <- mapM opExpr opFs
      let build (a : as) os = foldl (\l (o, r) -> atSpan (combineSrcSpans (getLocA l) (getLocA r)) (OpApp noExtField l o r)) a (zip os as)
          build [] _ = error "exprInfixChain"
      pure (build operands ops)
  | otherwise = failAt f "(:infix a op b ...) needs operands and operators alternating"

-- | An application spine: @(f a @T b)@.
appSpine :: Form -> Form -> [Form] -> P (LHsExpr GhcPs)
appSpine _ h args = do
  hd <- case h of
    _ | Just [x] <- kwForm "name" h -> do
          n <- nameAt NSExpr (Form (formPsSpan h) (FList [Form (formPsSpan h) (FKeyword (fsLit "name")), x]))
          pure (at h (HsVar noExtField n))
      | isKw "tuple-con" h || isKw "utuple-con" h -> failAt h "unexpected"
      | Just s <- symOf h, isSymbolic s -> do
          -- (op e): the operator applied to one argument
          n <- nameAt NSExpr h
          pure (at h (HsVar noExtField n))
      | otherwise -> expr EFun h
  let n = length args
  foldM (\acc (i, a) -> case a of
            Form _ (FPrefix PAt t) -> do
              ty <- typ TArg t
              pure (atSpan (combineSrcSpans (getLocA acc) (formSpan a)) (HsAppType noAnn acc (mkHsWildCardBndrs ty)))
            _ -> do
              e <- expr (if i == n then EArgLast else EArg) a
              pure (atSpan (combineSrcSpans (getLocA acc) (formSpan a)) (HsApp noExtField acc e)))
        hd (zip [1 :: Int ..] args)

recFieldsP :: (Form -> P (LocatedA arg)) -> [Form] -> P (HsRecFields GhcPs (LocatedA arg))
recFieldsP argP flds = recFieldsP' argP id flds

-- | The right-hand side GHC's parser gives a punned field in an expression.
punPlaceholder :: Form -> Form
punPlaceholder x = Form (formPsSpan x) (FAtom (ITvarid (fsLit "pun-right-hand-side")))

recFieldsP' :: (Form -> P (LocatedA arg)) -> (Form -> Form) -> [Form] -> P (HsRecFields GhcPs (LocatedA arg))
recFieldsP' argP punRhs flds = do
  let (fs, dd) = case reverse flds of
        (d : r) | isDotDot d -> (reverse r, Just d)
        _ -> (flds, Nothing)
  binds <- forM fs $ \x -> case tokForm (\case ITequal -> True; _ -> False) x of
    Just [l, r] -> do
      n <- nameAt NSExpr l
      v <- argP r
      pure (at x (HsFieldBind noAnn (at l (FieldOcc noExtField n)) v False))
    _ -> do
      n <- nameAt NSExpr x
      v <- argP (punRhs x)
      pure (at x (HsFieldBind noAnn (at x (FieldOcc noExtField n)) v True))
  pure (HsRecFields noAnn binds (fmap (\d -> at d (RecFieldsDotDot (length binds))) dd))

recUpdP :: [Form] -> P (LHsRecUpdFields GhcPs)
recUpdP flds
  | any isPath flds = do
      binds <- forM flds $ \x -> case tokForm (\case ITequal -> True; _ -> False) x of
        Just [l, r] -> do
          lbls <- pathOf l
          v <- expr ETop r
          pure (at x (HsFieldBind noAnn (at l (FieldLabelStrings lbls)) v False))
        _ -> do
          lbls <- pathOf x
          v <- expr ETop (last (fromMaybe [x] (kwForm "get" x)))
          pure (at x (HsFieldBind noAnn (at x (FieldLabelStrings lbls)) v True))
      pure (OverloadedRecUpdFields noExtField binds)
  where
    isPath x = isJust (kwForm "get" x) || maybe False (\case [l, _] -> isJust (kwForm "get" l); _ -> False) (tokForm (\case ITequal -> True; _ -> False) x)
    pathOf x = case kwForm "get" x of
      Just ls@(_ : _) -> NE.fromList <$> mapM (\l -> do { lbl <- fieldLabel l; pure (at l (DotFieldOcc noAnn (at l lbl))) }) ls
      _ -> failAt x "expected (:get field...)"
recUpdP flds = do
  binds <- forM flds $ \x -> case tokForm (\case ITequal -> True; _ -> False) x of
    Just [l, r] -> do
      n <- nameAt NSExpr l
      v <- expr ETop r
      pure (at x (HsFieldBind noAnn (at l (FieldOcc noExtField n)) v False))
    _ -> do
      n <- nameAt NSExpr x
      v <- expr ETop (punPlaceholder x)
      pure (at x (HsFieldBind noAnn (at x (FieldOcc noExtField n)) v True))
  pure (RegularRecUpdFields noExtField binds)

-------------------------------------------------------------------------------
-- Patterns

-- | Parse a pattern at a position, adding the implicit 'ParPat' (SPEC I5).
pat :: PPos -> Form -> P (LPat GhcPs)
pat pos f = do
  p <- patForm f
  po <- parenOptsP
  pure $ if patNeedsParens po pos (unLoc p)
    then at f (ParPat noAnn p)
    else p

patForm :: Form -> P (LPat GhcPs)
patForm f = case formNode f of
  FAtom t -> patAtom f t
  FVector [] -> conPat f [] f
  FVector xs -> at f . ListPat noAnn <$> mapM (pat PElem) xs
  FList [] -> conPat f [] f
  FPrefix PAt t -> at f . InvisPat (noAnn, SpecifiedSpec) . HsTP noExtField <$> typ TArg t
  FPrefix _ _ -> failAt f "unexpected prefix in a pattern"
  FKeyword _ -> failAt f "unexpected keyword in a pattern"
  FList _ | isNameForm f -> conPat f [] f
  FList [h] | isKw "tuple" h -> pure (at f (TuplePat noAnn [] Boxed))
            | isKw "utuple" h -> pure (at f (TuplePat noAnn [] Unboxed))
  FList [_] -> failAt f "a one-element list is not a pattern"
  FList (h : args) -> case h of
    _ | isTildeTok h, [p] <- args -> at f . LazyPat noAnn <$> pat PPrefixed p
      | isBangTok h, [p] <- args -> at f . BangPat noAnn <$> pat PPrefixed p
      | isKw "as" h, [n, p] <- args -> do
          nm <- varName' n
          at f . AsPat noAnn nm <$> pat PPrefixed p
      | isKw "paren" h, [p] <- args -> at f . ParPat noAnn <$> pat PParen p
      | isKw "tuple" h -> at f . (\ps -> TuplePat noAnn ps Boxed) <$> mapM (pat PElem) args
      | isKw "utuple" h -> at f . (\ps -> TuplePat noAnn ps Unboxed) <$> mapM (pat PElem) args
      | isKw "or" h, not (null args) -> at f . OrPat noExtField . NE.fromList <$> mapM (pat PTop) args
      | isKw "usum" h -> do
          let width = length args
          case [ (i, x) | (i, x) <- zip [1 ..] args, not (isHole x) ] of
            [(tag, x)] -> do
              p <- pat PElem x
              pure (at f (SumPat noAnn p tag width))
            _ -> failAt f "(:usum ...) needs exactly one non-:_ element"
      | isKw "rec" h, (c : flds) <- args -> do
          con <- nameAt NSExpr c
          fs <- recFieldsP' (pat PElem) punPlaceholder flds
          pure (at f (ConPat noExtField con (RecCon noAnn fs)))
      | isRArrow h, [e, p] <- args -> do
          e' <- expr ETop e
          p' <- pat PElem p
          pure (at f (ViewPat noAnn e' p'))
      | isDcolon h, [p, t] <- args -> do
          p' <- pat PSigSubj p
          t' <- typ TTop t
          pure (at f (SigPat noAnn p' (HsPS noAnn t')))
      | isTok (\case ITtype -> True; _ -> False) h, [t] <- args ->
          at f . EmbTyPat noAnn . HsTP noExtField <$> typ TTop t
      | isKw "mod" h, not (null args) -> do
          mods <- mapM modifierP (init args)
          at f . ModifiedPat noExtField mods <$> pat PTop (last args)
      | isKw "splice" h || isKw "qq" h -> at f . SplicePat noExtField <$> spliceForm f h args
      | isKw "qual-lit" h, [m, s] <- args -> at f . QualLitPat noExtField <$> qualLitP m s
      | isKw "infix" h -> patInfixChain f args
      | isMinusTok h, [x] <- args -> case x of
          Form _ (FAtom t) | Just ol <- overLitOf t ->
            pure (at f (NPat noAnn (at x ol) (Just noSyntaxExpr) noSyntaxExpr))
          _ -> failAt f "(- lit) needs a numeric literal"
      | isPlusTok h, [n, k] <- args, Form _ (FAtom t) <- k, Just ol <- overLitOf t -> do
          nm <- varName' n
          pure (at f (NPlusKPat noAnn nm (at k ol) ol noSyntaxExpr noSyntaxExpr))
      | Just s <- symOf h, symKind s == ConSym, length args >= 2 -> patConChain f h args
      | otherwise -> conPat f args h
  where
    isTildeTok = isTok (\case ITtilde -> True; ITvarsym s -> s == fsLit "~"; _ -> False)
    isBangTok = isTok (\case ITbang -> True; ITvarsym s -> s == fsLit "!"; _ -> False)
    isMinusTok = isTok (\case ITminus -> True; ITprefixminus -> True; ITvarsym s -> s == fsLit "-"; _ -> False)
    isPlusTok = isTok (\case ITvarsym s -> s == fsLit "+"; _ -> False)

overLitOf :: Token -> Maybe (HsOverLit GhcPs)
overLitOf = \case
  ITinteger il -> Just (mkHsIntegral il)
  ITrational fl -> Just (mkHsFractional fl)
  _ -> Nothing

patAtom :: Form -> Token -> P (LPat GhcPs)
patAtom f t = case t of
  ITunderscore -> pure (at f (WildPat noExtField))
  _ | Just ol <- overLitOf t -> pure (at f (NPat noAnn (at f ol) Nothing noSyntaxExpr))
    | Just l <- tokLit t -> pure (at f (LitPat noExtField l))
    | Just s <- tokSym t, symKind s `elem` [VarId, VarSym], Nothing <- symMod s -> at f . VarPat noExtField <$> nameAt NSExpr f
    | otherwise -> conPat f [] f

conPat :: Form -> [Form] -> Form -> P (LPat GhcPs)
conPat f args c = do
  con <- nameAt NSExpr c
  ps <- mapM (pat PArg) args
  pure (at f (ConPat noExtField con (PrefixCon noExtField ps)))

patConChain :: Form -> Form -> [Form] -> P (LPat GhcPs)
patConChain _ opF args = do
  operands <- mapM (pat POperand) args
  con <- nameAt NSExpr opF
  pure (foldlList (\l r -> atSpan (combineSrcSpans (getLocA l) (getLocA r)) (ConPat noExtField con (InfixCon noExtField l r))) operands)

patInfixChain :: Form -> [Form] -> P (LPat GhcPs)
patInfixChain f args
  | odd (length args), length args >= 3 = do
      let operandFs = [ x | (i, x) <- zip [0 :: Int ..] args, even i ]
          opFs = [ x | (i, x) <- zip [0 :: Int ..] args, odd i ]
      operands <- mapM (pat POperand) operandFs
      cons <- mapM (nameAt NSExpr) opFs
      case operands of
        (a : as) -> pure (foldl (\l (c, r) -> atSpan (combineSrcSpans (getLocA l) (getLocA r)) (ConPat noExtField c (InfixCon noExtField l r))) a (zip cons as))
        [] -> failAt f "empty chain"
  | otherwise = failAt f "(:infix a op b ...) needs operands and operators alternating"

-------------------------------------------------------------------------------
-- Arrow commands

cmd :: Form -> P (HsCmd GhcPs)
cmd f = case f of
  Form _ (FList (h : args)) -> case headOf h of
    HTok (ITlarrowtail _) | [a, b] <- args -> arrApp HsFirstOrderApp True a b
    HTok (ITLarrowtail _) | [a, b] <- args -> arrApp HsHigherOrderApp True a b
    HTok (ITrarrowtail _) | [a, b] <- args -> arrApp HsFirstOrderApp False b a
    HTok (ITRarrowtail _) | [a, b] <- args -> arrApp HsHigherOrderApp False b a
    HTok ITlam | (_ : _ : _) <- args -> do
      ps <- mapM (pat PArg) (init args)
      body <- lcmd (last args)
      let m = at f (Match noExtField (LamAlt LamSingle) (at f ps)
                    (GRHSs emptyComments (at f (GRHS noAnn [] body) :| []) (EmptyLocalBinds noExtField)))
      pure (HsCmdLam noAnn LamSingle (mkMatchGroup FromSource noAnn (at f [m])))
    HTok ITlcase -> do
      ms <- mapM (altP' (LamAlt LamCase) (const lcmd)) args
      pure (HsCmdLam noAnn LamCase (mkMatchGroup FromSource noAnn (at f ms)))
    HTok ITlcases -> do
      ms <- mapM (altP' (LamAlt LamCases) (const lcmd)) args
      pure (HsCmdLam noAnn LamCases (mkMatchGroup FromSource noAnn (at f ms)))
    HTok ITcase | (s : alts) <- args -> do
      scrut <- expr ETop s
      ms <- mapM (altP' CaseAlt (const lcmd)) alts
      pure (HsCmdCase noAnn scrut (mkMatchGroup FromSource noAnn (at f ms)))
    HTok ITif | [c, t, e] <- args -> HsCmdIf noAnn noSyntaxExpr <$> expr ETop c <*> lcmd t <*> lcmd e
    HTok ITlet | not (null args) -> HsCmdLet noAnn <$> localBindsP (init args) <*> lcmd (last args)
    HTok (ITdo _) -> do
      ss <- mapM cmdStmt args
      pure (HsCmdDo noAnn (at f ss))
    HKeyword k
      | k == fsLit "paren", [c] <- args -> HsCmdPar noAnn <$> lcmd c
      | k == fsLit "form", (op : cs) <- args -> do
          o <- expr ETop op
          tops <- mapM (\c -> at c . HsCmdTop noExtField <$> lcmd c) cs
          pure (HsCmdArrForm noAnn o Prefix tops)
      | k == fsLit "infix", [l, op, r] <- args -> do
          o <- opExpr op
          tops <- mapM (\c -> at c . HsCmdTop noExtField <$> lcmd c) [l, r]
          pure (HsCmdArrForm noAnn o Infix tops)
    _ | (_ : _) <- args -> do
          c <- case init args of
            [] -> lcmd h
            more -> lcmd (Form (formPsSpan f) (FList (h : more)))
          e <- expr EArgLast (last args)
          pure (HsCmdApp noExtField c e)
    _ -> failAt f "unexpected command"
  _ -> failAt f "expected a command"
  where
    lcmd x = case x of
      Form _ (FList [_]) -> failAt x "a one-element list is not a command"
      _ -> at x <$> cmd x
    arrApp ty rtl a b = do
      a' <- expr ETop a
      b' <- expr ETop b
      pure (HsCmdArrApp (NormalSyntax, noAnn) a' b' ty rtl)
    cmdStmt s = case s of
      Form _ (FList (h : as))
        | isLArrow h, [p, c] <- as -> do
            p' <- pat PTop p
            c' <- lcmd c
            pure (at s (BindStmt noAnn p' c'))
        | isTok (\case ITlet -> True; _ -> False) h -> at s . LetStmt noAnn <$> localBindsP as
        | isTok (\case ITrec -> True; _ -> False) h -> do
            ss <- mapM cmdStmt as
            pure (at s (RecStmt { recS_ext = noAnn, recS_stmts = at s ss, recS_later_ids = []
                                , recS_rec_ids = [], recS_bind_fn = noSyntaxExpr
                                , recS_ret_fn = noSyntaxExpr, recS_mfix_fn = noSyntaxExpr }))
      _ -> do
        c <- lcmd s
        pure (at s (BodyStmt noExtField c noSyntaxExpr noSyntaxExpr))

foldrList :: (a -> a -> a) -> [a] -> a
foldrList _ [x] = x
foldrList f (x : xs) = f x (foldrList f xs)
foldrList _ [] = error "foldrList: empty"

foldlList :: (a -> a -> a) -> [a] -> a
foldlList f (x : xs) = foldl f x xs
foldlList _ [] = error "foldlList: empty"
