{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | ghc-lisp tool modes: @ghc --hs2lisp@, @ghc --lisp2hs@ and
-- @ghc --lisp-check@ (the counterpart of go-lisp's @go tool golisp@).
module GHC.Driver.Lisp
  ( lispMode
  , parseHsFile
  , printLisp
  ) where

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Monad
import GHC.Driver.DynFlags
import GHC.Driver.Session (supportedLanguagesAndExtensions)
import GHC.Driver.Env.Types ()
import GHC.Platform (platformArchOS)
import GHC.Driver.Errors.Types
import GHC.Driver.Pipeline (preprocess)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Hs
import GHC.Hs.Dump
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
import GHC.Parser.Header (getOptionsFromFile)
import GHC.Parser.Lisp.Parens
import GHC.Parser.Lisp.Printer
import GHC.Parser.Lisp (parseLispModule, lispHeaderOptions)
import GHC.Driver.Session (parseDynamicFilePragma)
import GHC.Types.SourceText
import GHC.Data.Bag (bagToList)
import GHC.Parser.Errors.Types (PsMessage)
import GHC.Parser.Errors.Ppr ()
import GHC.Data.StringBuffer
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Types.Error
import GHC.Utils.Outputable
import GHC.Utils.Error
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (handle, SomeException)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Data.Data
import Data.List (isPrefixOf, isSuffixOf, sortOn, intercalate)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Hs.DocString
import Data.ByteString (ByteString)
import GHC.Types.Name (Name, getOccString)
import GHC.Types.Name.Occurrence
import GHC.Unit.Module (ModuleName, moduleNameString)
import Language.Haskell.Syntax.Text (HText, unpackHText)
import System.Exit
import System.IO

lispMode :: String -> [FilePath] -> Ghc ()
lispMode mode files = do
  hsc_env <- getSession
  case mode of
    "hs2lisp" -> forM_ files $ \f -> do
      (dflags, pragmas, m) <- liftIO (parseHsFile hsc_env f)
      liftIO (putStr (printLisp dflags pragmas m))
    "lisp2hs" -> forM_ files $ \f -> do
      (dflags, m) <- liftIO (parseLispFile hsc_env f)
      liftIO (putStr (printHaskell dflags m))
    "lisp-check" -> do
      results <- forM files $ \f -> liftIO (checkFile hsc_env f)
      let failed = length (filter not results)
      liftIO $ do
        hPutStrLn stderr (show (length results - failed) ++ " of " ++ show (length results) ++ " files round-trip")
        when (failed > 0) (exitWith (ExitFailure 1))
    _ -> liftIO $ do
      hPutStrLn stderr ("ghc --" ++ mode ++ ": not implemented yet")
      exitWith (ExitFailure 1)

-- | Parse a Haskell source file as GHC does: OPTIONS and LANGUAGE pragmas,
-- CPP, then the parser. Also returns the file-header options.
parseHsFile :: HscEnv -> FilePath
            -> IO (DynFlags, [String], Located (HsModule GhcPs))
parseHsFile hsc_env file = do
  pp <- preprocess hsc_env file Nothing Nothing
  case pp of
    Left msgs -> throwErrors (initSourceErrorContext (hsc_dflags hsc_env)) (GhcDriverMessage <$> msgs)
    Right (dflags0, hspp) -> do
      -- keep comments in the tree, so that hs2lisp can print them
      let dflags = gopt_set dflags0 Opt_KeepRawTokenStream
          popts = initParserOpts dflags
      (_, opts) <- getOptionsFromFile popts (initSourceErrorContext dflags)
                     (supportedLanguagesAndExtensions (platformArchOS (targetPlatform dflags))) file
      buf <- hGetStringBuffer hspp
      let loc = mkRealSrcLoc (mkFastString file) 1 1
      case unP parseModule (initParserState popts buf loc) of
        PFailed pst -> throwErrors (initSourceErrorContext dflags) (GhcPsMessage <$> getPsErrorMessages pst)
        POk pst m
          -- the parser can record errors and still return a tree
          | errs <- getPsErrorMessages pst, not (isEmptyMessages errs) ->
              throwErrors (initSourceErrorContext dflags) (GhcPsMessage <$> errs)
          | otherwise -> pure (dflags, map unLoc opts, m)

-- | Print a parsed module as Lisp, with its header pragmas and comments.
printLisp :: DynFlags -> [String] -> Located (HsModule GhcPs) -> String
printLisp dflags opts lm@(L _ m) =
  insertComments comments $
  renderWithContext ctx (lispHeaderPragmas exts others $$ text "" $$ lispModule popts m) ++ "\n"
  where
    ctx = initSDocContext dflags defaultUserStyle
    -- The module was parsed after CPP, so the Lisp file doesn't need it.
    exts = [ drop 2 o | o <- opts, "-X" `isPrefixOf` o, o /= "-XCPP" ]
    others = [ o | o <- opts, not ("-X" `isPrefixOf` o), o /= "-cpp" ]
    comments = moduleComments lm
    popts = PrintOpts (parenOpts dflags) (map fst comments) False

-- | The comments of a parsed Haskell module, in ghc-lisp syntax, with their
-- source positions. Haddock comments keep their meaning: @-- |@ becomes
-- @;;|@, @-- ^@ @;;^@, and so on; other comments become @;;@ comments.
-- File-header pragmas are left out (they are printed as forms).
moduleComments :: Located (HsModule GhcPs) -> [((Int, Int), [String])]
moduleComments m = sortOn fst [ (pos c, lisp (ac_tok (unLoc c))) | c <- collect m, keep (ac_tok (unLoc c)) ]
  where
    collect :: forall a. Data a => a -> [LEpaComment]
    collect x = case cast x of
      Just (c :: LEpaComment) -> [c]
      Nothing -> concat (gmapQ collect x)
    pos (L l _) = case l of
      EpaSpan (RealSrcSpan r _) -> (srcSpanStartLine r, srcSpanStartCol r)
      _ -> (0, 0)
    keep = \case
      EpaBlockComment t -> not ("{-#" `isPrefixOf` t)
      EpaLineComment _ -> True
      EpaDocComment _ -> True
      _ -> False
    lisp = \case
      -- GHC's rules: a line doc is "-- " and a decorator, a block doc "{-",
      -- an optional space and a decorator.
      EpaLineComment t -> case drop 2 t of
        ' ' : c : r | isDocSym c -> [";;" ++ c : r]
        r -> [";;" ++ plain r]
      EpaBlockComment t ->
        let body = dropEnd 2 (drop 2 t)
            (first, doc) = case body of
              c : r | isDocSym c -> (c : r, True)
              ' ' : c : r | isDocSym c -> (c : r, True)
              r -> (r, False)
        in case lines first of
             (l1 : ls) | doc -> (";;" ++ l1) : map ((";;" ++) . continuation) ls
                       | otherwise -> (";;" ++ plain l1) : map ((";;" ++) . plain) ls
             [] -> [";;"]
      -- with -haddock, doc comments are lexed as such
      EpaDocComment ds -> docLines ds
      _ -> []
    isDocSym c = c `elem` ("|^$*" :: String)
    -- a line of a nested doc comment that wouldn't continue a Lisp doc
    -- comment as it is is escaped with a backslash
    continuation r = case r of
      c : _ | isDocSym c || c == '-' || c == '\\' -> '\\' : r
      ' ' : '$' : _ -> '\\' : r
      _ -> r
    -- a plain comment must not start with a decorator in Lisp
    plain r = case r of
      c : _ | isDocSym c -> ' ' : r
      _ -> r
    docLines = \case
      MultiLineDocString _ dec (c :| cs) ->
        (";;" ++ decorator dec ++ chunk c) : map ((";;" ++) . chunk) cs
      NestedDocString _ dec c -> case lines (chunk c) of
        (l1 : ls) -> (";;" ++ decorator dec ++ l1) : map ((";;" ++) . continuation) ls
        [] -> [";;" ++ decorator dec]
      GeneratedDocString{} -> []
    chunk (L _ c) = unpackHDSC c
    decorator = \case
      HsDocStringNext -> "|"
      HsDocStringPrevious -> "^"
      HsDocStringNamed n -> "$" ++ n
      HsDocStringGroup n -> replicate n '*'
    dropEnd n xs = take (length xs - n) xs

-- | Put comments back in front of the printed lines they precede in the
-- source, using the position markers the printer leaves at the start of
-- lines (see 'GHC.Parser.Lisp.Printer.mark'), and remove the markers.
insertComments :: [((Int, Int), [String])] -> String -> String
insertComments comments0 = unlines . go comments0 . lines
  where
    go cs [] = concatMap snd cs
    go cs (l : ls) =
      let (ind, rest) = span (== ' ') l
          clean = ind ++ stripMarkers rest
      in case rest of
           '\1' : r | (p, _) <- marker r ->
             let (now, later) = span ((< p) . fst) cs
             in map (ind ++) (concatMap snd now) ++ clean : go later ls
           _ -> clean : go cs ls
    marker r = case break (== '\2') r of
      (pos, rest) -> case break (== ':') pos of
        (a, ':' : b) -> ((read a, read b), drop 1 rest)
        _ -> ((0, 0), rest)
    stripMarkers = \case
      '\1' : r -> stripMarkers (drop 1 (dropWhile (/= '\2') r))
      c : r -> c : stripMarkers r
      [] -> []

parenOpts :: DynFlags -> ParenOpts
parenOpts dflags = ParenOpts
  { poBlockArguments = xopt LangExt.BlockArguments dflags
  , poLexicalNegation = xopt LangExt.LexicalNegation dflags
  , poNegativeLiterals = xopt LangExt.NegativeLiterals dflags
  }

-- | Parse a ghc-lisp file: its header options, then the module.
parseLispFile :: HscEnv -> FilePath -> IO (DynFlags, Located (HsModule GhcPs))
parseLispFile hsc_env file = do
  buf <- hGetStringBuffer file
  let dflags0 = hsc_dflags hsc_env
      loc = mkRealSrcLoc (mkFastString file) 1 1
      opts = lispHeaderOptions (initParserOpts dflags0) buf loc
  (dflags, _, _) <- parseDynamicFilePragma (hsc_logger hsc_env) dflags0 opts
  case parseLispModule (initParserOpts dflags) buf loc of
    PFailed pst -> throwErrors (initSourceErrorContext dflags) (GhcPsMessage <$> getPsErrorMessages pst)
    POk _ m -> pure (dflags, m)

-- | Print a parsed module as Haskell with GHC's pretty-printer.
printHaskell :: DynFlags -> Located (HsModule GhcPs) -> String
printHaskell dflags (L _ m) = renderWithContext (initSDocContext dflags defaultUserStyle) (ppr m) ++ "\n"

-- | The corpus round trip for one Haskell file: parse it, print it as Lisp,
-- parse that, and compare the trees (SPEC.md §11).
checkFile :: HscEnv -> FilePath -> IO Bool
checkFile hsc_env file = handle (\(e :: SomeException) -> report ("ERROR " ++ takeWhile (/= '\n') (show e)) >> pure False) $ do
  (dflags, pragmas, m1) <- parseHsFile hsc_env file
  let txt = printLisp dflags pragmas m1
      loc = mkRealSrcLoc (mkFastString (file ++ ".hsl")) 1 1
      ctx = initSDocContext dflags defaultUserStyle
      dump m = astDump m
  case parseLispModule (initParserOpts dflags) (stringToStringBuffer txt) loc of
    PFailed pst -> do
      let errs = renderWithContext ctx (vcat (map psErr (bagToList (getMessages (getPsErrorMessages pst)))))
      report ("PARSE " ++ takeWhile (/= '\n') errs)
      save txt "" ""
      pure False
    POk _ m2 -> do
      let a1 = dump m1
          a2 = dump m2
      if a1 == a2
        then putStrLn ("OK " ++ file) >> pure True
        else do
          report ("DIFF " ++ firstDiff (lines a1) (lines a2))
          save txt a1 a2
          pure False
  where
    report msg = putStrLn ("FAIL " ++ file ++ ": " ++ msg) >> hFlush stdout
    save txt a1 a2 = do
      mdir <- lookupEnv "GHC_LISP_CHECK_OUT"
      forM_ mdir $ \dir -> do
        let base = dir </> map (\c -> if c == '/' then '_' else c) file
        writeFile (base ++ ".hsl") txt
        unless (null a1) $ writeFile (base ++ ".ast") a1
        unless (null a2) $ writeFile (base ++ ".ast.new") a2

psErr :: MsgEnvelope PsMessage -> SDoc
psErr e = ppr (errMsgSpan e) <> colon <+>
  formatBulleted (diagnosticMessage (defaultDiagnosticOpts @PsMessage) (errMsgDiagnostic e))

firstDiff :: [String] -> [String] -> String
firstDiff as bs = go (1 :: Int) as bs
  where
    go n (x : xs) (y : ys)
      | x == y = go (n + 1) xs ys
      | otherwise = "line " ++ show n ++ ": " ++ trim x ++ " /= " ++ trim y
    go n _ _ = "length differs at line " ++ show n
    trim = take 100 . dropWhile (== ' ')

-- | A structural dump of a syntax tree for the round-trip comparison
-- (SPEC.md §11): one constructor per line, with source positions,
-- exact-print annotations and the source text of pragma openers blanked.
astDump :: Data a => a -> String
astDump x0 = unlines (go 0 x0 [])
  where
    -- Lines are accumulated (go d x rest = the lines of x, then rest), so
    -- deep trees cost no more than wide ones.
    go :: forall b. Data b => Int -> b -> [String] -> [String]
    go d x rest
      | isAnnotation x = rest
      -- lists are flat, so that long lists don't nest (and indent) deeply
      | showConstr (toConstr x) == "(:)" = (indent d ++ "[") : listElems (d + 1) x rest
      | Just (s :: String) <- cast x = leaf (show s)
      | Just (fs :: FastString) <- cast x = leaf (show (unpackFS fs))
      | Just (t :: HText) <- cast x = leaf (show (unpackHText t))
      | Just (n :: Name) <- cast x = leaf ("Name " ++ getOccString n)
      | Just (o :: OccName) <- cast x = leaf ("Occ " ++ space o ++ " " ++ show (occNameString o))
      | Just (m :: ModuleName) <- cast x = leaf ("Module " ++ moduleNameString m)
      | Just (st :: SourceText) <- cast x = leaf $ case st of
          SourceText t | "{-#" `isPrefixOf` unpackFS t -> "NoSourceText"
          SourceText t -> "SourceText " ++ show (unpackFS t)
          NoSourceText -> "NoSourceText"
      | Just (ds :: HsDocString GhcPs) <- cast x = leaf ("DocString " ++ docText ds)
      | Just (r :: Rational) <- cast x = leaf (show r)
      | Just (b :: ByteString) <- cast x = leaf (show b)
      | otherwise =
          let c = toConstr x
          in case constrRep c of
               AlgConstr _ -> (indent d ++ showConstr c) : children (d + 1) x rest
               _ -> leaf (showConstr c)
      where
        leaf str = (indent d ++ str) : rest
    -- the children of x, each through go, then rest
    children :: forall b. Data b => Int -> b -> [String] -> [String]
    children d x rest = foldr (\f acc -> f acc) rest (gmapQ (\c -> go d c) x)
    listElems :: forall b. Data b => Int -> b -> [String] -> [String]
    listElems d x rest = case showConstr (toConstr x) of
      "(:)" -> gmapQi 0 (\h -> go d h) x (gmapQi 1 (\t -> listElems d t) x rest)
      _ -> rest
    -- past 100 levels the depth is written as a number
    indent d | d < 100 = replicate d ' '
             | otherwise = replicate 100 ' ' ++ show d ++ ":"
    space o
      | isVarOcc o = "v"
      | isTvOcc o = "tv"
      | isTcOcc o = "tc"
      | isDataOcc o = "d"
      | otherwise = "?"

-- | A doc string's decorator and text: nested (@{-| -}@) and line (@-- |@)
-- doc comments with the same text compare equal (SPEC.md N3).
docText :: HsDocString GhcPs -> String
docText = \case
  MultiLineDocString _ dec cs -> show dec ++ " " ++ show (intercalate "\n" (map (unpackHDSC . unLoc) (toList cs)))
  NestedDocString _ dec c -> show dec ++ " " ++ show (unpackHDSC (unLoc c))
  GeneratedDocString _ c -> "generated " ++ show (unpackHDSC c)

-- | Values that the round trip does not compare: positions and exact-print
-- annotations (DESIGN.md D13).
isAnnotation :: Typeable a => a -> Bool
isAnnotation x = annRep (typeOf x)
  where
    annRep rep =
      let name = tyConName (typeRepTyCon rep)
          args = typeRepArgs rep
      in annName name
         || (name `elem` containers && not (null args) && all annRep args)
    containers = ["List", "[]", "Maybe", "Either", "NonEmpty", "Tuple2", "Tuple3", "Tuple4", "(,)", "(,,)", "(,,,)"]
    annName name =
      name `elem` ["SrcSpan", "RealSrcSpan", "BufSpan", "IsUnicodeSyntax", "EpLayout", "NoEpAnns"]
      || "Ep" `isPrefixOf` name
      || ("Ann" `isPrefixOf` name && name `notElem` ["AnnDecl", "AnnProvenance"])
      || "Ann" `isSuffixOf` name || "Anns" `isSuffixOf` name
