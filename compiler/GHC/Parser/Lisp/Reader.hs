{-# LANGUAGE LambdaCase #-}

-- | The ghc-lisp reader: text -> forms.
--
-- The structure is EDN's (lists, vectors, @#_@ discard, @;@ comments, commas
-- as whitespace); every atom is exactly one Haskell lexeme, recognised by
-- GHC's own lexer with the file's extensions (DESIGN.md D2, SPEC.md §1).
module GHC.Parser.Lisp.Reader
  ( Form(..)
  , FormNode(..)
  , Prefix(..)
  , DocComment(..)
  , DocKind(..)
  , readForms
  , formSpan
  , formPsSpan
  ) where

import GHC.Prelude

import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Utils.Outputable

import Data.Char (isSpace, isAlpha)

-- | A form with the span of its source text.
data Form = Form { formPsSpan :: !PsSpan, formNode :: FormNode }

formSpan :: Form -> SrcSpan
formSpan = mkSrcSpanPs . formPsSpan

data FormNode
  = FList   [Form]
  | FVector [Form]
  | FAtom   Token            -- ^ one Haskell lexeme
  | FKeyword FastString      -- ^ @:name@, without the colon
  | FPrefix Prefix Form      -- ^ a prefix character attached to a form

data Prefix
  = PTick      -- ^ @'x@: promotion or TH value-name quote
  | PTyQuote   -- ^ @''T@: TH type-name quote
  | PAt        -- ^ @\@T@: type argument / invisible binder
  deriving (Eq, Show)

instance Outputable Form where
  ppr (Form _ n) = case n of
    FList fs     -> parens (hsep (map ppr fs))
    FVector fs   -> brackets (hsep (map ppr fs))
    FAtom t      -> text (show t)
    FKeyword k   -> char ':' <> ftext k
    FPrefix p f  -> text (case p of PTick -> "'"; PTyQuote -> "''"; PAt -> "@")
                    <> ppr f

-- | A Haddock comment (@;;|@, @;;^@, @;;*@, @;;$name@) and its continuation
-- lines (DESIGN.md D12).
data DocComment = DocComment
  { dcSpan  :: !PsSpan
  , dcKind  :: !DocKind
  , dcLines :: [String]
  }

data DocKind
  = DocNext            -- ^ @;;|@
  | DocPrev            -- ^ @;;^@
  | DocGroup !Int      -- ^ @;;*@, @;;**@, ...
  | DocNamed String    -- ^ @;;$name@
  deriving (Eq, Show)

type ReadErr = (PsSpan, String)

-- | Read all top-level forms of a file.
readForms :: ParserOpts -> StringBuffer -> RealSrcLoc
          -> Either ReadErr ([Form], [DocComment])
readForms opts buf0 rloc0 = go [] [] (skipShebang (PsLoc rloc0 (BufPos 0), buf0))
  where
    go acc docs st = case skipWs st of
      (docs', st')
        | atEnd (snd st') -> Right (reverse acc, reverse (docs' ++ docs))
        | otherwise -> do
            (f, st'') <- readForm opts st'
            go (maybe acc (: acc) f) (docs' ++ docs) st''

type St = (PsLoc, StringBuffer)

skipShebang :: St -> St
skipShebang st@(_, buf)
  | not (atEnd buf), currentChar buf == '#'
  , (_, b1) <- nextChar buf, not (atEnd b1), currentChar b1 == '!'
  = skipLine st
  | otherwise = st

step :: St -> (Char, St)
step (l, b) = let (c, b') = nextChar b in (c, (advancePsLoc l c, b'))

peek :: St -> Maybe Char
peek (_, b) | atEnd b = Nothing
            | otherwise = Just (currentChar b)

peek2 :: St -> Maybe Char
peek2 (_, b) | atEnd b = Nothing
             | (_, b') <- nextChar b, not (atEnd b') = Just (currentChar b')
             | otherwise = Nothing

skipLine :: St -> St
skipLine st = case peek st of
  Nothing   -> st
  Just '\n' -> st
  Just _    -> skipLine (snd (step st))

-- | Skip whitespace, commas and comments, collecting Haddock comments
-- (newest first).
skipWs :: St -> ([DocComment], St)
skipWs = loop []
  where
    loop docs st = case peek st of
      Just c | isSpace c || c == ',' -> loop docs (snd (step st))
      Just ';' -> case docComment st of
        Just (d, st') -> loop (d : docs) st'
        Nothing       -> loop docs (skipLine st)
      _ -> (docs, st)

-- | @;;|@ etc. at the current position, with continuation lines.
docComment :: St -> Maybe (DocComment, St)
docComment st0 = do
  let (l0, _) = st0
      (line0, stEnd0) = lineText st0
  kind_rest <- case line0 of
    ';':';':'|':r -> Just (DocNext, r)
    ';':';':'^':r -> Just (DocPrev, r)
    ';':';':'$':r -> let (n, r') = break isSpace r in Just (DocNamed n, r')
    ';':';':r@('*':_) -> let (stars, r') = span (== '*') r
                         in Just (DocGroup (length stars), r')
    _ -> Nothing
  let (kind, rest0) = kind_rest
      (ls, stEnd) = continuation [dropOneSpace rest0] stEnd0
  pure (DocComment (mkPsSpan l0 (fst stEnd)) kind (reverse ls), stEnd)
  where
    dropOneSpace (' ':r) = r
    dropOneSpace r = r
    -- Following lines that are only ";;" + text, with no marker, continue.
    continuation acc st = case peek st of
      Just '\n' ->
        let st1 = skipBlanks (snd (step st)) in
        case lineText st1 of
          (';':';':r, st2) | not (startsMarker r)
            -> continuation (dropOneSpace r : acc) st2
          _ -> (acc, st)
      _ -> (acc, st)
    startsMarker r = case r of
      c:_ -> c `elem` ("|^$*" :: String)
      _   -> False
    skipBlanks st = case peek st of
      Just c | c == ' ' || c == '\t' -> skipBlanks (snd (step st))
      _ -> st

-- | The rest of the line, and the state at its end (before the newline).
lineText :: St -> (String, St)
lineText st = case peek st of
  Nothing   -> ("", st)
  Just '\n' -> ("", st)
  Just _    -> let (c, st') = step st
                   (r, st'') = lineText st'
               in (c : r, st'')

isDelim :: Char -> Bool
isDelim c = isSpace c || c `elem` (",()[]{};\"`" :: String)

-- | Read one form. 'Nothing' for a form discarded by @#_@.
readForm :: ParserOpts -> St -> Either ReadErr (Maybe Form, St)
readForm opts st@(l0, _) = case peek st of
  Nothing -> Left (mkPsSpan l0 l0, "unexpected end of input")
  Just '(' -> seqForm ')' FList
  Just '[' -> seqForm ']' FVector
  Just c | c `elem` (")]}" :: String) ->
    Left (mkPsSpan l0 (fst (snd (step st))), "unexpected " ++ [c])
  Just '{' -> reserved "{ } maps are reserved"
  Just '`' -> reserved "backquotes are reserved"
  Just '#' | peek2 st == Just '_' -> do
    -- #_ form: discard
    let st2 = snd (step (snd (step st)))
        (_, st3) = skipWs st2
    (_, st4) <- readForm opts st3
    pure (Nothing, st4)
  Just '#' | peek2 st `elem` [Just '{', Just '#'] ->
    reserved "#{ } sets and #tags are reserved"
  Just ':' | Just c2 <- peek2 st, isAlpha c2 || c2 == '_' -> do
    let (_, st1) = step st
        (name, st2) = spanSt (not . isDelim) st1
    pure (Just (Form (mkPsSpan l0 (fst st2)) (FKeyword (fsLit name))), st2)
  Just '@' | Just c2 <- peek2 st, not (isDelim c2) || c2 `elem` ("([" :: String) ->
    prefix PAt 1
  Just '\'' | peek2 st == Just '\'' -> prefix PTyQuote 2
  _ -> atom
  where
    reserved msg = Left (mkPsSpan l0 (fst (snd (step st))), msg)

    seqForm close mk = do
      let (_, st1) = step st
      let loop acc s = case skipWs s of
            (_, s') -> case peek s' of
              Nothing -> Left (mkPsSpan l0 l0, "unclosed " ++ openOf close)
              Just c | c == close ->
                let (_, s'') = step s'
                in Right (Just (Form (mkPsSpan l0 (fst s'')) (mk (reverse acc))), s'')
              _ -> do
                (f, s'') <- readForm opts s'
                loop (maybe acc (: acc) f) s''
      loop [] st1
    openOf ')' = "("
    openOf _   = "["

    prefix p n = do
      let st1 = iterate (snd . step) st !! n
      case peek st1 of
        Just c | not (isDelim c) || c `elem` ("([" :: String) -> pure ()
        _ -> Left (mkPsSpan l0 (fst st1), "a prefix must be followed by a form")
      (mf, st2) <- readForm opts st1
      case mf of
        Nothing -> Left (mkPsSpan l0 (fst st2), "a prefix cannot apply to #_")
        Just f  -> pure (Just (Form (mkPsSpan l0 (fst st2)) (FPrefix p f)), st2)

    atom = case lexOne opts st of
      Left e -> Left e
      Right (tok, st1)
        -- A tick that GHC's lexer didn't take as a char literal is a prefix.
        | ITsimpleQuote <- tok -> prefix PTick 1
        | ITtyQuote <- tok -> prefix PTyQuote 2
        -- -5 without NegativeLiterals: the reader produces (- 5).
        | isMinus tok, Just c <- peek st1, not (isDelim c) -> do
            (mf, st2) <- readForm opts st1
            case mf of
              Just f@(Form _ (FAtom t)) | isNumeric t ->
                let sp = mkPsSpan l0 (fst st2)
                    minus = Form (mkPsSpan l0 (fst st1)) (FAtom tok)
                in pure (Just (Form sp (FList [minus, f])), st2)
              _ -> Left (mkPsSpan l0 (fst st2),
                         "a symbol must be one Haskell lexeme; insert spaces")
        | Just c <- peek st1, not (isDelim c) ->
            Left (mkPsSpan l0 (fst (skipToDelim st1)),
                  "a symbol must be one Haskell lexeme; insert spaces")
        | otherwise ->
            pure (Just (Form (mkPsSpan l0 (fst st1)) (FAtom tok)), st1)

    skipToDelim s = case peek s of
      Just c | not (isDelim c) -> skipToDelim (snd (step s))
      _ -> s

spanSt :: (Char -> Bool) -> St -> (String, St)
spanSt p st = case peek st of
  Just c | p c -> let (_, st') = step st
                      (r, st'') = spanSt p st'
                  in (c : r, st'')
  _ -> ("", st)

isMinus :: Token -> Bool
isMinus = \case
  ITminus -> True
  ITprefixminus -> True
  ITvarsym s -> s == fsLit "-"
  _ -> False

isNumeric :: Token -> Bool
isNumeric = \case
  ITinteger{} -> True
  ITrational{} -> True
  ITprimint{} -> True
  ITprimword{} -> True
  ITprimfloat{} -> True
  ITprimdouble{} -> True
  ITprimint8{} -> True
  ITprimint16{} -> True
  ITprimint32{} -> True
  ITprimint64{} -> True
  ITprimword8{} -> True
  ITprimword16{} -> True
  ITprimword32{} -> True
  ITprimword64{} -> True
  _ -> False

-- | Lex exactly one Haskell token starting at the current position.
lexOne :: ParserOpts -> St -> Either ReadErr (Token, St)
lexOne opts (l@(PsLoc rl _), buf) =
  let pst0 = (initParserState opts buf rl) { loc = l }
  in case unP (lexer False return) pst0 of
       PFailed pst -> Left (mkPsSpan l (loc pst), "lexical error")
       POk pst (L sp tok)
         | ITeof <- tok -> Left (mkPsSpan l l, "unexpected end of input")
         | RealSrcSpan rsp _ <- sp, realSrcSpanStart rsp /= rl ->
             Left (mkPsSpan l l, "unexpected comment or layout")
         -- \case and \cases are two tokens in GHC's lexer: \ then case,
         -- which becomes ITlcase after ITlam.
         | ITlam <- tok, not (atEnd (buffer pst)), not (isDelim (currentChar (buffer pst)))
         , POk pst' (L _ tok') <- unP (lexer False return) pst
         , isLamCase tok' -> Right (tok', (endLoc pst', buffer pst'))
         | otherwise -> Right (tok, (endLoc pst, buffer pst))
  where
    endLoc pst = psSpanEnd (last_loc pst)

isLamCase :: Token -> Bool
isLamCase = \case
  ITlcase -> True
  ITlcases -> True
  _ -> False
