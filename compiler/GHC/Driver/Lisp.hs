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
import Data.List (isPrefixOf, isSuffixOf)
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
    Right (dflags, hspp) -> do
      let popts = initParserOpts dflags
      (_, opts) <- getOptionsFromFile popts (initSourceErrorContext dflags)
                     (supportedLanguagesAndExtensions (platformArchOS (targetPlatform dflags))) file
      buf <- hGetStringBuffer hspp
      let loc = mkRealSrcLoc (mkFastString file) 1 1
      case unP parseModule (initParserState popts buf loc) of
        PFailed pst -> throwErrors (initSourceErrorContext dflags) (GhcPsMessage <$> getPsErrorMessages pst)
        POk _ m -> pure (dflags, map unLoc opts, m)

-- | Print a parsed module as Lisp, with its header pragmas.
printLisp :: DynFlags -> [String] -> Located (HsModule GhcPs) -> String
printLisp dflags opts (L _ m) =
  renderWithContext ctx (lispHeaderPragmas exts others $$ text "" $$ lispModule popts m) ++ "\n"
  where
    ctx = initSDocContext dflags defaultUserStyle
    exts = [ drop 2 o | o <- opts, "-X" `isPrefixOf` o, o /= "-XCPP" ]
    others = [ o | o <- opts, not ("-X" `isPrefixOf` o) ]
    popts = PrintOpts (parenOpts dflags)

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
astDump = unlines . go 0
  where
    go :: forall b. Data b => Int -> b -> [String]
    go d x
      | isAnnotation x = []
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
      | Just (r :: Rational) <- cast x = leaf (show r)
      | Just (b :: ByteString) <- cast x = leaf (show b)
      | otherwise =
          let c = toConstr x
          in case constrRep c of
               AlgConstr _ -> (indent ++ showConstr c) : concat (gmapQ (go (d + 1)) x)
               _ -> leaf (showConstr c)
      where
        indent = replicate d ' '
        leaf str = [indent ++ str]
    space o
      | isVarOcc o = "v"
      | isTvOcc o = "tv"
      | isTcOcc o = "tc"
      | isDataOcc o = "d"
      | otherwise = "?"

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
