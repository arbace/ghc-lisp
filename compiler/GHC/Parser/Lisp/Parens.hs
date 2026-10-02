{-# LANGUAGE LambdaCase #-}

-- | Implicit parentheses (SPEC.md I5, §9).
--
-- Lisp nesting shows structure, so the Lisp parser inserts 'HsPar',
-- 'HsParTy' and 'ParPat' exactly where Haskell's grammar needs parentheses,
-- and the Lisp printer leaves out exactly those. Both sides use the
-- functions here, so they agree by construction; the corpus round trip
-- checks that the rules match what Haskell sources contain.
--
-- Each node has a /level/ (how tightly it binds) and each position a
-- /required level/. A node is parenthesized when its level is below the
-- position's. Block forms (lambda, let, if, case, do, ...) extend as far
-- to the right as possible, so they also need parentheses in any position
-- that is not the last thing in its enclosing construct.
module GHC.Parser.Lisp.Parens
  ( ParenOpts(..)
  , EPos(..), exprNeedsParens
  , PPos(..), patNeedsParens
  , TPos(..), typeNeedsParens
  ) where

import GHC.Prelude

import GHC.Hs hiding (patNeedsParens)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
import GHC.Data.FastString

data ParenOpts = ParenOpts
  { poBlockArguments  :: !Bool
  , poLexicalNegation :: !Bool
  , poNegativeLiterals :: !Bool
  }

-- | Expression positions.
data EPos
  = ETop        -- ^ nothing needed: bodies, list and tuple elements, ...
  | EParen      -- ^ directly inside @(:paren ...)@
  | ESigSubj    -- ^ @e@ in @e :: T@
  | EOpFirst    -- ^ first operand of an operator chain
  | EOpMid      -- ^ middle operand
  | EOpLast     -- ^ last operand
  | EFun        -- ^ function in an application
  | EArg        -- ^ argument, not the last
  | EArgLast    -- ^ last argument
  | ENegArg     -- ^ operand of negation
  | ESectionL   -- ^ operand of a left section
  | ESectionR   -- ^ operand of a right section
  | EAtom       -- ^ @e.field@, @e { .. }@, @static e@, @$e@
  deriving (Eq, Show)

exprNeedsParens :: ParenOpts -> EPos -> HsExpr GhcPs -> Bool
exprNeedsParens opts pos e
  | isBlock e = not (openOk pos)
  | otherwise = exprLevel opts e < required pos
  where
    required = \case
      ETop -> 0
      EParen -> -1
      ESigSubj -> 1
      EOpFirst -> 2
      EOpMid -> 3
      EOpLast -> if poLexicalNegation opts then 2 else 3
      EFun -> 3
      EArg -> 4
      EArgLast -> 4
      ENegArg -> 3
      ESectionL -> 1
      ESectionR -> 1
      EAtom -> 4
    openOk = \case
      ETop -> True
      EParen -> True
      EOpLast -> True
      ESectionR -> True
      EArgLast -> poBlockArguments opts
      _ -> False

-- | Block forms: they extend as far to the right as possible.
isBlock :: HsExpr GhcPs -> Bool
isBlock = \case
  HsLam{} -> True
  HsCase{} -> True
  HsIf{} -> True
  HsMultiIf{} -> True
  HsLet{} -> True
  HsDo _ flav _ -> not (isComprehension flav)
  HsProc{} -> True
  _ -> False

isComprehension :: HsDoFlavour -> Bool
isComprehension = \case
  ListComp -> True
  MonadComp -> True
  _ -> False

exprLevel :: ParenOpts -> HsExpr GhcPs -> Int
exprLevel opts = \case
  HsVar{} -> 4
  HsOverLabel{} -> 4
  HsIPVar{} -> 4
  HsOverLit _ ol -> overLitLevel opts ol
  HsLit _ l -> litLevel opts l
  HsQualLit{} -> 4
  HsPar{} -> 4
  ExplicitTuple{} -> 4
  ExplicitSum{} -> 4
  ExplicitList{} -> 4
  ArithSeq{} -> 4
  RecordCon{} -> 4
  RecordUpd{} -> 4
  HsGetField{} -> 4
  HsProjection{} -> 4
  HsTypedBracket{} -> 4
  HsUntypedBracket{} -> 4
  HsTypedSplice{} -> 4
  HsUntypedSplice{} -> 4
  HsHole{} -> 4
  HsStar{} -> 4
  HsDo _ flav _ | isComprehension flav -> 4
  HsApp{} -> 3
  HsAppType{} -> 3
  HsStatic{} -> 3
  HsPragE{} -> 3
  NegApp{} -> 2
  HsLam{} -> 2
  HsCase{} -> 2
  HsIf{} -> 2
  HsMultiIf{} -> 2
  HsLet{} -> 2
  HsDo{} -> 2
  HsProc{} -> 2
  OpApp{} -> 1
  ExprWithTySig{} -> 0
  HsEmbTy{} -> 0
  HsForAll{} -> 0
  HsQual{} -> 0
  HsFunArr{} -> 0
  SectionL{} -> -1
  SectionR{} -> -1

overLitLevel :: ParenOpts -> HsOverLit GhcPs -> Int
overLitLevel opts ol = case ol_val ol of
  HsIntegral il | il_neg il -> negLevel opts
  HsFractional fl | fl_neg fl -> negLevel opts
  _ -> 4

litLevel :: ParenOpts -> HsLit GhcPs -> Int
litLevel opts l = case l of
  HsIntPrim (SourceText t) _ | negText t -> negLevel opts
  HsInt8Prim (SourceText t) _ | negText t -> negLevel opts
  HsInt16Prim (SourceText t) _ | negText t -> negLevel opts
  HsInt32Prim (SourceText t) _ | negText t -> negLevel opts
  HsInt64Prim (SourceText t) _ | negText t -> negLevel opts
  HsFloatPrim _ fl | fl_neg fl -> negLevel opts
  HsDoublePrim _ fl | fl_neg fl -> negLevel opts
  _ -> 4
  where
    negText t = take 1 (unpackFS t) == "-"

-- | A negative literal binds like an atom under NegativeLiterals.
negLevel :: ParenOpts -> Int
negLevel opts = if poNegativeLiterals opts then 4 else 2

-- | Pattern positions.
data PPos
  = PTop        -- ^ case alternative, bind statement, binding lhs, ...
  | PParen      -- ^ directly inside @(:paren ...)@
  | PArg        -- ^ constructor or function argument, lambda argument
  | POperand    -- ^ operand of an infix constructor chain
  | PPrefixed   -- ^ inside @~p@, @!p@, @x\@p@
  | PSigSubj    -- ^ @p@ in @p :: T@
  deriving (Eq, Show)

patNeedsParens :: ParenOpts -> PPos -> Pat GhcPs -> Bool
patNeedsParens opts pos p = patLevel opts p < required
  where
    required = case pos of
      PTop -> 0
      PParen -> -1
      PArg -> 4
      POperand -> 3
      PPrefixed -> 4
      PSigSubj -> 1

patLevel :: ParenOpts -> Pat GhcPs -> Int
patLevel opts = \case
  WildPat{} -> 4
  VarPat{} -> 4
  LazyPat{} -> 4
  AsPat{} -> 4
  ParPat{} -> 4
  BangPat{} -> 4
  ListPat{} -> 4
  TuplePat{} -> 4
  SumPat{} -> 4
  SplicePat{} -> 4
  InvisPat{} -> 4
  QualLitPat{} -> 4
  LitPat _ l -> litLevel opts l
  NPat _ (L _ ol) mneg _
    | Just _ <- mneg -> negLevel opts
    | otherwise -> overLitLevel opts ol
  ConPat { pat_args = args } -> case args of
    PrefixCon _ [] -> 4
    PrefixCon{} -> 3
    RecCon{} -> 4
    InfixCon{} -> 1
  NPlusKPat{} -> 1
  SigPat{} -> 0
  EmbTyPat{} -> 0
  ModifiedPat{} -> 0
  OrPat{} -> -1
  ViewPat{} -> -1

-- | Type positions.
data TPos
  = TTop        -- ^ signature body, arrow result, forall and context bodies
  | TParen      -- ^ directly inside @(:paren ...)@, tuple elements
  | TArrowArg   -- ^ left of @->@
  | TOperand    -- ^ operand of a type operator chain
  | TFun        -- ^ head of a type application
  | TArg        -- ^ argument of a type application
  | TCtxElem    -- ^ a constraint in a context
  | TSigSubj    -- ^ @t@ in @t :: k@
  deriving (Eq, Show)

typeNeedsParens :: TPos -> HsType GhcPs -> Bool
typeNeedsParens pos t = typeLevel t < required
  where
    required = case pos of
      TTop -> 0
      TParen -> -1
      TArrowArg -> 2
      TOperand -> 3
      TFun -> 3
      TArg -> 4
      TCtxElem -> 2
      TSigSubj -> 1

typeLevel :: HsType GhcPs -> Int
typeLevel = \case
  HsForAllTy{} -> 0
  HsQualTy{} -> 0
  HsFunTy{} -> 1
  HsOpTy{} -> 2
  HsAppTy{} -> 3
  HsAppKindTy{} -> 3
  HsKindSig{} -> -1
  HsIParamTy{} -> -1
  HsDocTy _ (L _ t) _ -> typeLevel t
  _ -> 4
