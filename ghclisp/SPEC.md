# ghc-lisp syntax specification (draft)

This is the normative companion to `DESIGN.md`. For every constructor of
the parsed syntax tree (`HsModule GhcPs`, `compiler/Language/Haskell/Syntax/*`
plus `GHC.Hs.*`) it gives:
- the canonical Lisp form,
- the rule that tells it apart from every other form,
- how it is compared in the round-trip test.

It relies on decisions D1-D16 (see DESIGN.md). Points marked **[S#]** are
choices made while writing this spec, on top of the D-items; all were
accepted on 2026-10-02 (see §12).

Notation: `x?` means optional, `x*` zero or more, `x+` one or more.

## 0. Global invariants

- **I1. Haskell order.** Every form lists its components in Haskell's
  source order. Only the head moves to the front: operators (`(+ a b)`),
  reserved operators used as heads (`(:: f T)`, `(= lhs rhs)`,
  `(-> pat rhs)`, `(<- p e)`), and keywords. Positions therefore increase in
  the same order as in Haskell.
- **I2. Classification is by context, head and shape, never by type or
  scope.** The parser decides what a form is from:
  - the syntactic context (module header, top-level declaration, class or
    instance body, local bindings, statement, expression, pattern, type,
    constructor declaration, export item, import item, command, ...);
  - the head (a special symbol, a keyword, or anything else);
  - the number and token class of the elements;
  - the lexical class of a name (variable, constructor, variable operator,
    constructor operator), exactly as Haskell distinguishes `f x = ...`
    (a function binding) from `Just x = ...` (a pattern binding);
  - the file's language extensions, but only where Haskell's own lexer or
    parser depends on them (`MagicHash`, `NegativeLiterals`,
    `LexicalNegation`, `BlockArguments`, `StarIsType`, `NPlusKPatterns`,
    and words that are keywords only under an extension: `proc`, `rec`,
    `mdo`, `static`, `pattern`, `role`, `family`, ...). A word is special
    exactly where it is special in Haskell **[S1]**.
- **I3. Special heads.** A list whose head is one of the following is a
  special form, never an application:
  - a Haskell keyword: `case class data default deriving do else foreign if
    import in infix infixl infixr instance let module newtype of then type
    where`, `forall`, and the extension keywords of I2 in their contexts;
  - a reserved operator: `:: = | <- -> => \ ~ @ ..` and `\case`, `\cases`;
    in a command context also `-< -<< >- >>-`;
  - any EDN keyword (`:tuple`, `:infix`, ...).

  `!`, `~` and `-` are special only where Haskell gives them a special
  meaning (bang and lazy patterns, strictness annotations, negation);
  elsewhere they are ordinary operators (`(! arr i)`, `(~ a b)` in a type).
  To use a special symbol as an ordinary name, write `(:name sym)`:
  `((:name ->) r)` is the type `((->) r)`. **[S2]**
- **I4. No nullary application.** A list with exactly one element is an
  error in every expression, pattern and type position: `(f)` is not
  "call f". Write `f`, or `(:paren f)` for Haskell's `(f)`. (Special forms
  with one argument, like `(- x)`, have two elements.) **[S3]**
- **I5. Implicit parentheses.** Lisp nesting already shows the structure,
  so the parser inserts `HsPar` / `HsParTy` / `ParPat` (and their kin)
  exactly where Haskell's grammar needs parentheses for the same tree:
  minimal parentheses. For example `(f (g x))` gets `HsPar` around `g x`,
  and `(* (+ a b) c)` gets one around `a + b`, but `(>>= m (\ x (k x)))`
  leaves the trailing lambda bare, as Haskell does. Parentheses that the
  Haskell source has beyond that minimum are written `(:paren e)`. The
  printer emits `(:paren ...)` for exactly those, so the round trip is
  exact. The position table is in §9. **[S4]**
- **I6. One AST, one canonical form.** Where two Lisp spellings parse to
  the same AST (`(Maybe a)` / `((:name Maybe) a)`), the printer emits the
  first one listed here.

## 1. Lexical syntax (reader)

| Token | Rule |
|---|---|
| whitespace | space, tab, newline, CR, form feed, and **comma** |
| `;` comment | to the end of the line. `;;|`, `;;^`, `;;*`, `;;$name` start Haddock comments (§10). A first line starting with `#!` is skipped, as GHC does. |
| `#_ form` | discards the next form (EDN) |
| `( )` `[ ]` | list, vector |
| `{ }` `#{ }` `#tag` | read, but **reserved**: the parser rejects them |
| `` ` `` | reserved (error) |
| string `"..."` | Haskell string grammar exactly, including `\&`, `\^A`, `\SOH`, and gaps (`\   \`). The literal keeps its source text verbatim. `"..."#` with `MagicHash` is a primitive string. Multiline strings (`"""`) are not read yet (D2). |
| char `'x'` | Haskell char grammar. `'x'#` is a primitive char. |
| number | Haskell number grammar exactly: decimal, `0x`, `0o`, `0b` (`BinaryLiterals`), `_` separators (`NumericUnderscores`), floats, hex floats (`HexFloatLiterals`), `MagicHash` suffixes `#` / `##`, and `ExtendedLiterals` suffixes (`123#Int8`). Source text verbatim. A leading `-` (`-5`) is a negative literal under `NegativeLiterals`, and otherwise reads as `(- 5)`. |
| keyword | `:` then a letter or `_`, then name characters: `:tuple`, `:_`, `:section-l`. Used as heads, the `:_` marker, and flags. |
| symbol | any other run of non-delimiter characters. It must be exactly one Haskell lexeme of these kinds, possibly qualified with module segments (`M.N.x`, `M.+`, `M.:|`): variable `x`, `foldl'`, `x#` (`MagicHash`); constructor `Just`; variable operator `+`, `>>=`, `.`; constructor operator `:`, `:|`, `:+:`; reserved operators. Anything else is an error, so `a+b`, `f.g` and `x.y` are errors ("insert spaces", or `(:get x y)`, D15). |
| prefix `'` | `'Just`, `'[]`, `'[a b]`, `'(:tuple a b)`, `':` are promoted names and forms; `'f` and `''T` are Template Haskell name quotes. The reader tells a char literal from a prefix tick as GHC's lexer does: `'x'` is a char. |
| prefix `@` | `@T`, `@(Maybe a)`, `@_`: a type argument or invisible binder (`TypeApplications`, `TypeAbstractions`), attached to the next form with no space, as in Haskell. |
| prefix `?` | `?x` is an implicit parameter (`ImplicitParams`). |
| prefix `#` | `#foo` and `#"foo bar"` are overloaded labels (`OverloadedLabels`). `#_` always means discard; a label starting with `_` is written `#"_x"`. |
| `_`, `..`, `()`, `[]` | wildcard / hole; dotdot; unit; nil. They mean whatever Haskell's `_`, `..`, `()` and `[]` mean in that context. |

Unicode: letters, digits and symbols follow GHC's lexer (`GHC.Parser.CharClass`).
`UnicodeSyntax` alternatives (`→`, `∷`, `∀`, `⊸`) are accepted and read
as their ASCII forms; they only differ in exact-print annotations.

## 2. Files and modules

```
file   := header-pragma* module-head? top*
header-pragma := (:language Ext+) | (:options-ghc "flags"+) | (:options-haddock "flags"+)
module-head   := (module M warning? exports?)
top    := (import ...) | decl            ; imports first, as in Haskell
```

| Node | Lisp | Notes |
|---|---|---|
| `HsModule` | the whole file | **[S5]** The module head is a short form, `(module Data.Shape [exports])`, and imports and declarations follow at top level (as go-lisp's `(package p)`), rather than nesting inside it. |
| `hsmodName` = Nothing | no `module` form | as in Haskell |
| `hsmodExports` | `[item*]` after the name; omitted = Nothing; `[]` = `Just []` | |
| `hsmodDeprecMessage` | `(:deprecated "msg")`, `(:warning in? "cat"? "msg")` before the exports | the same shapes as §6 warning pragmas |
| `hsmodHaddockModHeader` | `;;|` comment before `(module ...)` | |
| header pragmas | `(:language ...)`, `(:options-ghc ...)`, `(:options-haddock ...)` before everything else | D10. Not part of the AST; the downsweep reads them. `CPP` is rejected (D11). |

**Export items** (`IE`):

| Node | Lisp |
|---|---|
| `IEVar` / `IEThingAbs` | `f`, `+`, `T`, `:+:` (which one follows GHC's rule for the name's namespace) |
| `IEThingAll` | `(T ..)` |
| `IEThingWith` | `(T A b)`, with a wildcard: `(T A .. b)`; `T()` is `(T :_)` **[S6]** |
| `IEModuleContents` | `(module M)` |
| `IEWrappedName` | `(pattern P)`, `(type +)`, `(data X)`, `(default C)`; combined: `(type (T ..))`, `(pattern (P ..))` |
| `IEWholeNamespace` | `(type ..)`, `(data ..)` |
| `NamespaceSpecifier` in `IEThingAll` | `(type (T ..))` |
| `IEGroup`, `IEDoc`, `IEDocNamed` | `;;*` / `;;**` heading, `;;|` doc, `;;$name` inside the vector |
| `ExportDoc` | `;;^` after the item |

**Imports** (`ImportDecl`): the words of Haskell's import, in Haskell's
order, with `{-# SOURCE #-}` written `:source`:

```
(import :source? safe? level? qualified? "pkg"? M level? qualified? (as N)? (hiding? [item*])?)
```

- `(import qualified Data.Map as M)` is `QualifiedPre`, and
  `(import Data.Map qualified as M)` is `QualifiedPost`. `level` is `splice`
  or `quote` (`ExplicitLevelImports`), before or after the module name
  (`LevelStylePre` / `LevelStylePost`). **[S7]** `as N` is two symbols, not
  a list: `(import M as N)`.
- The import list is a vector `[item*]` with the export-item forms;
  `hiding [item*]` is `EverythingBut`.

## 3. Declarations

### 3.1 Bindings and signatures (D7)

| Node | Lisp | Disambiguation |
|---|---|---|
| `FunBind` (equation) | `(= (f p*) rhs)`, `(= f rhs)`, `(= (:infix p1 op p2) rhs)`, `(= ((:infix p1 op p2) p*) rhs)` | The lhs head is a variable (or variable operator in `:infix`). Adjacent equations for the same name merge into one `FunBind`, exactly as `getMonoBind` does (only equations with arguments merge). `mc_fixity` is `Infix` for `:infix` lhs. |
| strict variable binding | `(= (! x) rhs)` | `FunBind` with `mc_strictness = SrcStrict`, as `!x = e` in Haskell |
| `PatBind` | `(= pat rhs)` where pat is not a variable or variable application: `(= (Just x) e)`, `(= (:tuple a b) e)`, `(= [a b] e)`, `(= (~ p) e)`, `(= (:infix x : xs) e)` | lexical class of the head, as in Haskell |
| `PatBind` modifiers | `(= (:mod m* pat) rhs)` | |
| rhs | `expr`, or guards `(| qual* expr)+`, then `(where binding*)?` | A `(| ...)` element is a guarded rhs; its last element is the body and the others are the guard's qualifiers. `where` is last. |
| `PatSynBind` | `(pattern lhs = pat)`, `(pattern lhs <- pat)`, `(pattern lhs <- pat (where (= ...)+))` | lhs: `(P a b)`, `(:infix a :< b)`, `(:rec P a b)` |
| implicit-parameter binding (`IPBind`) | `(= ?x e)` in `let` / `where` | |
| `TypeSig` | `(:: f g T)`, `(:mod m* (:: f T))` | names, then the type (last element) |
| `PatSynSig` | `(pattern (:: P Q T))` | |
| `ClassOpSig` | `(:: f T)` in a class body; `(default (:: f T))` is the default signature | |
| `FixSig` | `(infixl 6 + -)`, `(infixr 5 type :+:)` | precedence optional, as in Haskell |
| `InlineSig` | `(:inline act? :conlike? f)`, likewise `:noinline`, `:inlinable`, `:opaque` | `act` is `[2]`, `[~2]`, `[~]` (§6) |
| `SpecSig` | `(:specialise :inline? act? f T+)` | name, then one or more types |
| `SpecSigE` | `(:specialise :inline? act? (forall ...)? e)` | one expression after the options |
| `SpecInstSig` | `(:specialise instance T)` | |
| `MinimalSig` | `(:minimal bf)`, `bf := name \| (:or bf+) \| (:and bf+) \| (:paren bf)` | `Parens` per I5 |
| `SCCFunSig` | `(:scc f)`, `(:scc f "label")` | |
| `CompleteMatchSig` | `(:complete A B)`, `(:complete A B :: T)` | |

Spellings: `:specialise`/`:specialize` and `:inlinable`/`:inlineable` are
accepted; the source text of pragma openers is not compared (§11, N2).

### 3.2 Type-level declarations

Declaration heads: `T`, `(T tv*)`, or `(:infix tv1 op tv2)` for
`data a :+: b`. Type variable binders: `a`, `(:: a K)`, `@a` / `@(:: a K)`
(`HsBndrInvisible`), `_` (`HsBndrWildCard`). A kind signature on the head
wraps it: `(:: (T a) K)`. **[S8]**

| Node | Lisp | Notes |
|---|---|---|
| `DataDecl` | `(data :ctype? head-or-ctx con* deriving*)` | `head-or-ctx` is the head, or `(=> ctx+ head)` for a datatype context |
| H98 `ConDeclH98` | `C`, `(C T*)`, `(:infix T1 :+ T2)`, `(C (:: f g T)+)` (record), with existentials `(forall tv+ (=> ctx+ con))` | D5. A field type may be wrapped in `(! T)`, `(~ T)`, `(:unpack T)`, `(:nounpack T)`, and `;;^` docs follow it. Multiplicity in a record field: `(:: f (:mod m T))`. |
| GADT body | `(where (:: C+ type)*)` instead of `con*` | `ConDeclGADT`. The type is `(forall ... (=> ctx (-> arg* res)))`. A record GADT constructor: `(:: C (-> (:record (:: f T)+) res))`. |
| `type data` | `(type data head con*)` | `DataTypeCons True` |
| `newtype` | `(newtype head-or-ctx con deriving*)` | one constructor |
| `HsDerivingClause` | `(deriving strategy? C)`, `(deriving strategy? [C*])`, `(deriving [C*] via T)` | `C` alone is `DctSingle`, a vector is `DctMulti`. strategy: `stock`, `newtype`, `anyclass`. |
| `SynDecl` | `(type head T)` | |
| `StandaloneKindSig` | `(type (:: T K))` | the second element is a `::` form |
| `ClassDecl` | `(class head-or-ctx fundeps? body*)` | fundeps: `(| (a b -> c)+)`, each with a `->` marker. Body: signatures, default signatures, bindings, fixity, pragmas, associated types (`(type head)`, `(data head)`, `(type family ...)`), associated defaults (`(type head T)`, `(type instance (= ...))`), docs |
| `ClsInstDecl` | `(instance overlap? T body*)` | `T` is the instance head with its context and forall, e.g. `(=> (Eq a) (Eq [a]))`. overlap: `:overlapping`, `:overlappable`, `:overlaps`, `:incoherent`, `:no-overlap`, `:noncanonical` |
| `FamilyDecl` (open) | `(type family head result? inj?)`, `(data family head result?)` | result: `(:: head K)` on the head, or `(= r)` / `(= (:: r K))`; injectivity: `(| r -> a b)` |
| closed family | `(type family head result? inj? (where eqn*))`, `(where ..)` for abstract | |
| `FamEqn` | `(= (F T*) rhs)`, `(forall tv* (= (F T*) rhs))` | type arguments may be `@k` |
| `TyFamInstDecl` | `(type instance eqn)` | |
| `DataFamInstDecl` | `(data instance head con* deriving*)`, `(newtype instance ...)` | head is `(F T*)`, optionally `(forall tv* head)` |
| `DerivDecl` | `(deriving strategy? instance overlap? T)`, `(deriving via V instance T)` | |
| `DefaultDecl` | `(default T*)`; named defaults: `(default C [T*])` **[S9]** | |
| `ForeignDecl` | `(foreign import cconv safety? "entity"? (:: f T))`, `(foreign export cconv "entity"? (:: f T))` | cconv: `ccall capi stdcall prim javascript`; safety: `safe unsafe interruptible` |
| `RoleAnnotDecl` | `(type role T r*)`, r: `nominal representational phantom _` | |
| `WarnDecls` | `(:deprecated ns? name+ msg)`, `(:warning in? "cat"? ns? name+ msg)` | msg: a string or a vector of strings; ns: `type` or `data` |
| `AnnDecl` | `(:ann f e)`, `(:ann type T e)`, `(:ann module e)` | |
| `RuleDecls` | `(:rules rule*)`, rule: `("name" act? (forall tv*)? (forall bndr*)? (= lhs rhs))` | With one `forall`, it binds terms; with two, the first binds types, as in Haskell. bndr: `x` or `(:: x T)` |
| `SpliceDecl` | `(:splice e)` (`DollarSplice`); a bare expression at top level (`BareSplice`) | a top-level list whose head isn't special is a naked splice, as in Haskell |
| `DocDecl` | free-standing `;;|`, `;;^`, `;;*`, `;;$name` comments | |

Modifiers on declarations (`%m data T`, ...): `(:mod m* decl)`.

## 4. Expressions

| Node | Lisp | Notes |
|---|---|---|
| `HsVar` | `x`, `M.x`, `Just`, `+` (as a value), `(:name sym)` | |
| `HsOverLabel` | `#foo`, `#"foo"` | |
| `HsIPVar` | `?x` | |
| `HsOverLit`, `HsLit` | literal tokens | Which one follows GHC's parser (numbers are overloaded, chars, strings and primitive literals are `HsLit`). |
| `HsQualLit` | `(:qual-lit M "text")` | `QualifiedStrings` |
| `HsLam` | `(\ p+ e)`, `(\case alt*)`, `(\cases alt*)` | a lambda's patterns are every element before the last |
| `HsApp` | `(f a b)` | left-nested: `((f a) b)`. A head that is an operator symbol is §4.1. |
| `HsAppType` | `(f @T x)` | |
| `OpApp`, `NegApp`, `SectionL/R` | §4.1 | |
| `HsPar` | implicit (I5), or `(:paren e)` | |
| `ExplicitTuple` | `(:tuple e+)`, with `:_` for `Missing`; `(:utuple e*)` | `(:tuple e)` is a one-tuple `MkSolo` only if GHC's parser can produce it; otherwise an error |
| `ExplicitSum` | `(:usum :_* e :_*)` | the position of `e` is the tag, and the length is the width |
| `HsCase` | `(case e alt*)`, alt: `(-> pat rhs)` | rhs: an expression, or guards and `where` as in §3.1 |
| `HsIf` | `(if c t e)` | |
| `HsMultiIf` | `(if (| qual* e)+)` | `if` whose arguments are all `(| ...)` clauses |
| `HsLet` | `(let binding+ e)` | the last element is the body; the others are binding forms |
| `HsDo` | `(do stmt+)`, `(mdo stmt+)`, `(M.do ...)`, `(M.mdo ...)` | |
| list comprehension | `[e | qual+]`, parallel `[e | qual+ | qual+]` | a vector whose second element is `|`; `MonadComp` under `MonadComprehensions`, as in Haskell |
| `ExplicitList` | `[e*]` | |
| `ArithSeq` | `[a ..]`, `[a b ..]`, `[a .. c]`, `[a b .. c]` | a vector with `..` in second or third place |
| `RecordCon` | `(:rec C field*)`, field: `(= f e)`, `f` (pun), `..` (last) | |
| `RecordUpd` | `(:update e (= f e)+)`; `OverloadedRecordUpdate` paths: `(= (:get f g) e)` | |
| `HsGetField` | `(:get e f+)` | `(:get e a b)` is `e.a.b` (nested) |
| `HsProjection` | `(:proj f+)` | `(.a.b)` |
| `ExprWithTySig` | `(:: e T)` | in expression context |
| `HsTypedBracket` | `(:typed-quote e)` | D16 |
| `HsUntypedBracket` | `(:quote e)`, `(:quote-pat p)`, `(:quote-type T)`, `(:quote-decls d*)`, `'f`, `''T` | `DecBrL`; `VarBr` |
| `HsTypedSplice` | `(:typed-splice e)` | |
| `HsUntypedSplice` | `(:splice e)`, `(:qq quoter "text")` | |
| `HsProc` | `(proc pat cmd)` | §8 |
| `HsStatic` | `(static e)` | `StaticPointers` |
| `HsPragE` (SCC) | `(:scc "label" e)`, `(:scc label e)` | |
| `HsEmbTy` | `(type T)` | |
| `HsStar` | `*` where GHC's parser produces it | |
| `HsHole` | `_` | |
| `HsForAll`, `HsQual`, `HsFunArr` | `(forall ...)`, `(=> ...)`, `(-> a b)`, `(->. a b)` in expression context | as for types (§7); `RequiredTypeArguments` |

### 4.1 Operators (D4)

| Lisp | Haskell / AST |
|---|---|
| `(op a b c ...)`, 2+ operands | the chain `a op b op c`: `OpApp`s nested to the left, no `HsPar` between links, fixity resolved by the renamer |
| `(:infix a op1 b op2 c ...)` | a mixed chain, left-nested; a name in an operator slot is a backtick operator (``a `div` b``) |
| `(- e)` | `NegApp` |
| `(op e)`, one operand, `op` not `-` | `HsApp (HsVar op) e`, i.e. `(op) e` |
| `((:name op) a b)` | `(op) a b`, two `HsApp`s |
| `(:section-l e op)` / `(:section-r op e)` | `SectionL` / `SectionR` (with the `HsPar` that a section always has) |

Operands are parenthesized per I5: `(* (+ a b) c)` is `(a + b) * c`.

### 4.2 Statements

| Node | Lisp | Notes |
|---|---|---|
| `BindStmt` | `(<- pat e)` | |
| `LetStmt` | `(let binding+)` | in statement context, a `let` whose elements are all bindings |
| `BodyStmt` / `LastStmt` | `e` | the last statement becomes `LastStmt` as GHC's parser makes it |
| `RecStmt` | `(rec stmt+)` | `RecursiveDo` |
| `ParStmt` | the `|` groups of a comprehension | |
| `TransStmt` | `(then f)`, `(then f by e)`, `(then group using f)`, `(then group by e using f)` | `TransformListComp` |

## 5. Patterns

| Node | Lisp |
|---|---|
| `WildPat` | `_` |
| `VarPat` | `x` |
| `LazyPat` | `(~ p)` |
| `AsPat` | `(:as x p)` |
| `ParPat` | implicit (I5), or `(:paren p)` |
| `BangPat` | `(! p)` |
| `ListPat` | `[p*]` |
| `TuplePat` | `(:tuple p*)`, `(:utuple p*)` |
| `OrPat` | `(:or p+)` |
| `SumPat` | `(:usum :_* p :_*)` |
| `ConPat` prefix | `C`, `(C @T* p*)` |
| `ConPat` infix | `(op p1 p2 ...)` chain, `(:infix p1 op p2 ...)` (as §4.1) |
| `ConPat` record | `(:rec C (= f p)* f* ..?)` |
| `ViewPat` | `(-> e p)` |
| `SplicePat` | `(:splice e)`, `(:qq q "text")` |
| `LitPat`, `NPat` | literal tokens; `(- 5)` is a negated `NPat` |
| `NPlusKPat` | `(+ n 1)` under `NPlusKPatterns` |
| `SigPat` | `(:: p T)` |
| `EmbTyPat` | `(type T)` |
| `InvisPat` | `@t` |
| `ModifiedPat` | `(:mod m* p)` |
| `QualLitPat` | `(:qual-lit M "text")` |

## 6. Pragmas and activations

Activations: `[2]` = `ActiveAfter 2`, `[~2]` = `ActiveBefore 2`, `[~]` =
`NeverActive`, none = `AlwaysActive`. (`[~2]` reads `~2` as one token in
this position.)

Pragma forms are EDN keywords named after the pragma, in lower case:
`:inline :noinline :inlinable :opaque :specialise :minimal :complete :scc
:ann :rules :deprecated :warning :unpack :nounpack :ctype :source
:overlapping :overlappable :overlaps :incoherent :no-overlap :noncanonical
:language :options-ghc :options-haddock`.

## 7. Types and kinds

| Node | Lisp | Notes |
|---|---|---|
| `HsForAllTy` | `(forall tv+ T)` invisible, `(forall tv+ -> T)` visible | inferred binders: `(:inferred tv)` = `{a}` |
| `HsQualTy` | `(=> ctx* T)` | the last element is the body; `(=> T)` is `() => T` |
| `HsTyVar` | `a`, `Maybe`, `M.T`, `'Just`, `+` (as a type) | |
| `HsAppTy` | `(Maybe a)` | |
| `HsAppKindTy` | `(T @k a)` | |
| `HsFunTy` | `(-> a b c)` (right-nested), `(->. a b)` (linear), `(-> (:mod m* a) b)` (modified) | the modifiers sit on the argument, in source order |
| `HsListTy` | `[T]` | exactly one element |
| `HsTupleTy` | `()`, `(:tuple A B ...)`, `(:utuple ...)` | |
| `HsSumTy` | `(:usum A B ...)` | |
| `HsOpTy` | `(op A B ...)` chain (right-nested, as GHC's parser builds type chains), `(:infix A op B ...)`, promoted `(': A B)` | `(~ a b)` is an equality constraint |
| `HsParTy` | implicit (I5), or `(:paren T)` | |
| `HsIParamTy` | `(:: ?x T)` | in a type context |
| `HsStarTy` | `*` | `StarIsType` |
| `HsKindSig` | `(:: T K)` | |
| `HsSpliceTy` | `(:splice e)`, `(:qq q "text")` | |
| `HsDocTy` | a type followed by `;;^` | |
| `HsExplicitListTy` | `'[T*]`, or `[T T+]` (two or more elements, unpromoted) | |
| `HsExplicitTupleTy` | `'(:tuple T*)` | |
| `HsTyLit` | `"sym"`, `42`, `'c'` | |
| `HsWildCardTy` | `_` | |
| tuple / list / arrow constructors as names | `(:tuple-con n)`, `(:utuple-con n)`, `[]`, `()`, `(:name ->)` | the same in expressions and patterns |

## 8. Arrow commands

| Node | Lisp |
|---|---|
| `HsCmdArrApp` | `(-< f x)`, `(-<< f x)`, `(>- x f)`, `(>>- x f)` |
| `HsCmdArrForm` | `(:form e cmd*)` (banana brackets), `(:infix c1 op c2)` |
| `HsCmdApp` | `(cmd e)` |
| `HsCmdLam` | `(\ p+ cmd)`, `(\case alt*)`, `(\cases alt*)` |
| `HsCmdPar` | implicit, or `(:paren cmd)` |
| `HsCmdCase`, `HsCmdIf`, `HsCmdLet`, `HsCmdDo` | `(case e alt*)`, `(if c c1 c2)`, `(let b+ cmd)`, `(do stmt+)` |

## 9. Implicit parentheses (I5)

The parser parenthesizes a node in these positions when Haskell needs it:

| Position | Parenthesized | Not parenthesized |
|---|---|---|
| application argument | applications, operator forms, negation, signatures, sections, lambdas and other block forms | atoms, tuples, lists, records, brackets, splices; block forms as the last argument under `BlockArguments` |
| function in an application | operator forms, negation, signatures, block forms | applications (left nesting) |
| operator operand | operator forms, signatures; block forms except in the last operand; negation except in the first operand (or under `LexicalNegation`) | applications, atoms |
| `::` subject | signatures, block forms | applications, operator forms |
| type argument | type applications, arrows, operators, foralls, contexts, kind signatures | atoms, tuples, lists |
| arrow argument (left of `->`) | arrows, foralls, contexts | applications, operators |
| pattern argument | constructor applications with arguments, operator patterns, signatures, view patterns, negated literals | atoms, `!p`, `~p`, `x@p` |

This table is the starting point. The corpus round trip is the judge: any
file whose parentheses differ is either a parser bug or a missing row.
The printer omits `HsPar` exactly where the parser would add it, and
prints `(:paren ...)` otherwise.

## 10. Haddock comments (D12)

`;;|` (next), `;;^` (previous), `;;*`, `;;**`, ... (group headings), and
`;;$name` (named chunk). Following lines that start with `;;` and no marker
continue the comment. They become `HsDocString`s where GHC's Haddock pass
would put the corresponding `--` comment, so `-haddock` sees the same tree.

## 11. Round-trip comparison

The corpus test parses a Haskell file, prints it as Lisp, parses that, and
compares the two `HsModule GhcPs` with
`showAstData BlankSrcSpan BlankEpAnnotations` after `eraseEpLayout`, as
`utils/check-ppr` does, plus these normalizations:

- **N1.** Positions and exact-print annotations are not compared (D13).
- **N2.** The source text of pragma openers (`{-# INLINE`, `{-# SCC`,
  `{-# SOURCE`, overlap modes, rules, warnings, `MINIMAL`, `COMPLETE`) is
  not compared: pragmas are case-insensitive and spaced freely in Haskell.
  The Lisp parser fills in the canonical spelling.
- **N3.** Nested doc comments (`{-| -}`) compare equal to line comments
  with the same text.
- **N4.** Files that need features not read yet (multiline strings,
  CPP-dependent text) are skipped and counted.

Literal source text is compared exactly (D2).

## 12. Spec-level choices (accepted 2026-10-02)

| ID | Choice |
|---|---|
| S1 | Extension-dependent words and lexemes follow Haskell's rules exactly. |
| S2 | `(:name sym)` writes a special symbol or operator as a plain name. |
| S3 | One-element lists are errors in expressions, patterns and types. |
| S4 | Parentheses are implicit where Haskell needs them; extra ones are `(:paren ...)`. |
| S5 | `(module M [exports])` is a header form; the declarations follow at top level. |
| S6 | `T()` in an export list is `(T :_)`. |
| S7 | Imports use Haskell's words in Haskell's order (`qualified`, `as`, `hiding`, `safe`, `splice`, `quote`, `:source`). |
| S8 | Declaration heads are `(T tv*)`; kind signatures wrap them as `(:: head K)`. |
| S9 | Named defaults are `(default C [T*])`. |
