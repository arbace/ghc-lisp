# ghc-lisp: design notes (living document)

Status: **draft v3**. The syntax is not settled. Every open decision has an
ID (D1, D2, ...) so we can talk about it and update it. Settled decisions
move to the "Decided" log at the bottom. Sister project: go-lisp
(github.com/arbace/go-lisp), whose decisions are the starting point here.

## 1. Goal

Add a second front end to GHC. It accepts the whole of GHC Haskell written
as s-expressions, in a Clojure/EDN flavor, in `.hsl` files. The rest of the
compiler (renamer, typechecker, desugarer, Core, STG, Cmm, codegen) must not
notice which syntax a module used.

## 2. Guiding principles

1. **Same AST, different reader.** The Lisp parser produces the same
   `HsModule GhcPs` (`compiler/Language/Haskell/Syntax/*`, `compiler/GHC/Hs/*`)
   as `GHC.Parser`. No separate semantics, no macro expansion inside the
   compiler. Haskell keeps its semantics exactly because the renamer sees an
   identical tree.
2. **Every Haskell module has a Lisp form, and the mapping can be reversed.**
   Every `GhcPs` AST can be printed as Lisp and parsed back to an equal AST
   (compared like `utils/check-ppr` does: `showAstData BlankSrcSpan
   BlankEpAnnotations`, after the normalizations listed in SPEC). Every Lisp
   module can be printed as Haskell with GHC's own `ppr`. This gives us:
   - a corpus test: `libraries/`, `compiler/`, `utils/` and the
     `testsuite/tests` files that parse
     (Haskell -> AST -> Lisp -> AST', then check AST == AST'),
   - `hs2lisp` / `lisp2hs` converters for free,
   - an escape hatch for tools that only understand `.hs` files.
3. **Every valid Haskell name must still be expressible**, including
   operators, primes, `MagicHash` names and qualified names. A special-form
   head that is also a legal Haskell identifier would make some programs
   impossible to write (see D1).
4. **Small footprint in upstream files.** New code goes in new modules and
   directories. Edits to existing upstream files are one-line hooks, marked
   `ghc-lisp:` in a comment, so merging from upstream GHC stays easy.
5. **Stay close to EDN**, and make every addition deliberate (D2).

## 3. Architecture

```
Foo.hsl ─┐
         ├─ GHC.Parser.Lisp.parseModule ──┐
Foo.hs ──┴─ GHC.Parser.parseModule ───────┴──> HsModule GhcPs ──> rename ──> typecheck ──> ...
```

- New modules under `compiler/GHC/Parser/Lisp/` (names provisional):
  - `Reader.hs`: text -> forms (list, vector, map, set, symbol, keyword,
    string, char, number), each with a `RealSrcSpan`.
  - `Parser.hs` (exported as `GHC.Parser.Lisp`): forms -> `GhcPs` nodes, in
    the `P` monad so errors and warnings are ordinary `PsMessage`s. Positions
    go into `SrcSpan`s; exact-print annotations get `noAnn` or the minimum
    that later passes need (D13).
  - `Printer.hs`: `GhcPs` AST -> Lisp text (the `hs2lisp` direction).
  - Each new module is added to `compiler/ghc.cabal.in` (one block, marked).
- Hooks, all to be marked `ghc-lisp:`:
  - `GHC/Driver/Main/Passes.hs` (`hscParse'`): choose the parser from the
    file's suffix. This is the analog of go-lisp's `noder.go` hook.
  - `GHC/Driver/Phases.hs`: `.hsl` is a Haskell source suffix.
  - `GHC/Unit/Finder.hs`: search `.hsl` next to `.hs`/`.lhs`.
  - `GHC/Parser/Header.hs`: the downsweep reads the module name, imports
    and `LANGUAGE`/`OPTIONS_GHC` pragmas *before* the full parse
    (`getImports`, `getOptionsFromFile`). For `.hsl` this calls the Lisp
    reader. This is the analog of go-lisp's `internal/golisp` header reader.
- Test harness: `utils/check-lisp`, modeled on `utils/check-ppr`, plus
  testsuite tests. A later phase: `cabal` support (`libraries/Cabal` is a
  submodule, so that work happens upstream of this repo or as a patch).
- Haddock, HLS, `ghc-exactprint` stay Haskell-only. Until they support Lisp,
  tools can run on the converted `.hs` output.

## 4. Syntax sketch (v2; follows the decided D-items, the rest is open)

```clojure
(:language LambdaCase ScopedTypeVariables)      ; {-# LANGUAGE ... #-}   (D10)

(module Data.Shape [Shape (..) area mkSquare]   ; module Data.Shape (Shape(..), area, mkSquare) where
                                                ; names verbatim (D3)
  (import Data.List [sortBy])                   ; import Data.List (sortBy)
  (import qualified Data.Map.Strict as M)       ; Haskell's own words (D9)
  (import Prelude hiding [lookup])

  (data Shape                                   ; data Shape = Circle Double | Rect { w, h :: Double }
    (Circle Double)
    (Rect (:: w h Double))                      ; record fields (D5)
    (deriving Show Eq))

  (:: area (-> Shape Double))                   ; area :: Shape -> Double
  (= (area (Circle r)) (* pi r r))              ; area (Circle r) = pi * r * r   (D4: chain)
  (= (area (Rect w h)) (* w h))

  (:: classify (=> [(Ord a) (Num a)] (-> a String)))
  (= (classify n)                               ; guards and where (D7)
    (| (< n 0) "negative")
    (| (== n 0) "zero")
    (| otherwise big)
    (where (= big "positive")))

  (:: main (IO ()))
  (= main
    (do (<- xs (fmap lines getContents))        ; xs <- lines <$> getContents
        (let (= n (length xs)))
        (print (:tuple n (sortBy (comparing negate) [1 2 3])))   ; (D5, D6)
        (mapM_ (\case (-> 0 (pure ())) (-> k (print k))) [n])
        (print (:infix 1 + 2 * 3)))))           ; mixed chain, fixity left to the renamer (D4)
```

## 5. Open decisions

**D1. Heads.** Decided (2026-10-02): EDN keywords. See the log.

**D2. Lexemes.** EDN structure (lists, vectors, `#_`; `{}`, `#{}`, `#tag`
reserved) plus Haskell's own literal grammar: strings with Haskell escapes
(`\&`, `\SOH`, `\^A`, string gaps), char literals `'a'`, numbers with
`NumericUnderscores`, hex floats, binary literals, `MagicHash` suffixes
(`3#`, `3##`, `"x"#`). Identifiers may contain `'` (`foldl'`, `x'`), which
the reader has to tell apart from char literals and from promotion /
TH name quotes (`'Just`, `''T`). Multiline strings (`"""`) maybe later.

**D3. Names.** Decided (2026-10-02): verbatim. See the log.

**D4. Operators and fixity.** Decided (2026-10-02): chains plus `:infix`;
sections are `:section-l` / `:section-r`. See the log.

**D5. Vectors.** Decided (2026-10-02): Haskell brackets; record fields
are `(:: fields... Type)` groups. See the log.

**D6. Tuples.** Decided (2026-10-02): `(:tuple ...)` and `()`. See the log.

**D7. Bindings.** Decided (2026-10-02): `(= lhs rhs)` forms. See the log.

**D8. Layout.** S-expressions replace layout: `do`, `let`, `where`, `case`,
`class`, `instance`, `\case` bodies are the form's remaining elements.
No braces, no semicolons.

**D9. Vocabulary.** Decided (2026-10-02): Haskell words. See the log.

**D10. Pragmas.** Decided (2026-10-02): forms before `module`. See the
log.

**D11. CPP.** Options:
  - (a) no CPP in `.hsl` for now. Converting a CPP'd `.hs` file captures one
    configuration; the corpus test works on the post-CPP AST either way.
  - (b) run CPP on `.hsl` files as on `.hs`. EDN's `;` comments and `'`
    chars may confuse the C preprocessor, even in traditional mode.

**D12. Comments and Haddock.** Haddock comments are part of the AST (with
`-haddock`). Plain comments are only in EPA. Milestone 1: drop plain
comments, maybe keep doc comments as `;;|` / `;;^`. Revisit with
`hs2lisp`.

**D13. Positions and exact-print annotations.** `GhcPs` carries EPA
(`EpAnn`, `EpToken`, ...) for `ghc-exactprint`. The Lisp parser fills real
`SrcSpan`s from forms, and leaves annotations empty unless a later pass
reads them. The round-trip test ignores annotations (as check-ppr does).

**D14. Scope.** The end goal is all of GHC's surface syntax, including
GADTs, type families, Template Haskell, quasiquotes, arrows, linear types,
`RequiredTypeArguments`, unboxed sums. Proposed order: Haskell 2010
plus the extensions that `libraries/` uses, then everything that
`testsuite/tests` parses.

**D15. Dots.** Qualified names (`M.insert`, `Data.Map.Map`, `M.!`) and
`OverloadedRecordDot` (`r.field`, `r.a.b`) share the dot. Haskell itself
separates them by case: an uppercase segment before the dot is a module.
Proposal: dotted symbols follow the same rule; `(:get e field)` selects
from a non-name expression.

**D16. Template Haskell and quasiquotes.** Splices `(:splice e)` / `$x`?,
quotes `(:quote e)`, typed variants, name quotes `'f` / `''T`, and
quasiquotes `(:qq name "raw text")`, whose body stays text.

## 6. Roadmap

0. Settle D1..D16 with worked examples. Write `ghclisp/SPEC.md` with one
   form per AST constructor.
1. Reader with positions, plus tests.
2. Printer (AST -> Lisp) first: it forces a complete mapping for every
   constructor and generates the corpus.
3. Parser (Lisp -> AST), plus `utils/check-lisp` and the corpus round trip.
4. Driver hooks: `ghc Foo.hsl`, hello world, `--make` with mixed `.hs` /
   `.hsl` modules. Then a behavioral test: testsuite run-tests converted to
   `.hsl` behave identically.
5. Tooling: `hs2lisp` / `lisp2hs`, error messages, editor mode, cabal.

## Decided

- **(2026-10-02) Branch and extension.** Work happens on the `ghc-lisp`
  branch; `master` mirrors upstream GHC. The file extension is `.hsl`.
- **D3 (2026-10-02): names are verbatim.** A Lisp name is the Haskell name,
  spelled the same: `sortBy`, `foldl'`, `M.insert`, `:|`, `x#`. The mapping
  is the identity: no kebab-case, no exemption lists, and error messages
  already match the source. (go-lisp's D18 doesn't apply: Haskell gets
  meaning from case and lists exports explicitly.)
- **D4 (2026-10-02): operator forms are unresolved chains.** GHC's parser
  doesn't know fixities and the renamer re-associates operator chains, so:
  - `(op a b c ...)` with two or more operands is the Haskell chain
    `a op b op c`, left unresolved for the renamer. `(++ a b c)` groups to the
    right, `(- a b c)` to the left, and `(== a b c)` is a fixity error, all as
    in Haskell.
  - `(:infix a + b * c)` is a mixed chain, exactly Haskell's `a + b * c`.
    The operators sit in the odd positions; a plain name there is a
    backtick operator: `(:infix x div y)` is ``x `div` y``.
  - An operator form nested as an operand is wrapped in `HsPar`, so the Lisp
    grouping always wins: `(* (+ a b) c)` is `(a + b) * c`.
  - `(- x)` with one operand is negation (`NegApp`).
  - An operator used as a value is the bare symbol: `(foldr + 0 xs)`.
- **D5 (2026-10-02): vectors are Haskell brackets.** `[1 2 3]` is a list
  literal, `[x y]` a list pattern, and `[Int]` the list type. Vectors also
  group syntax where the position makes that unambiguous (export and
  import lists, for example); lists are used elsewhere.
- **D10 (2026-10-02): file-header pragmas are forms before `module`.**
  `(:language GADTs LambdaCase)`, `(:options-ghc "-Wall")`, ... The
  downsweep reads them with the Lisp reader. Pragmas inside the code
  (`INLINE`, `RULES`, `UNPACK`, ...) are forms as well.
- **D4 sections (2026-10-02).** `(:section-l a op)` is `(a op)` and
  `(:section-r op b)` is `(op b)`; a plain name as `op` is a backtick
  operator: `(:section-r div 2)` is ``(`div` 2)``. `(+ 1)` stays an ordinary
  application of `(+)` to `1`. Note: Haskell reads `(- 1)` as negation,
  not a section, except under `LexicalNegation`; `(:section-r - 1)` is the
  section that `LexicalNegation` writes as `(- 1)`. SPEC decides whether it
  is accepted without that extension.
- **D5 record fields (2026-10-02).** A record constructor's field groups
  reuse the signature form: `(Rect (:: w h Double) (:: name String))` is
  `Rect { w, h :: Double, name :: String }`. A constructor with no `::`
  group is positional, so `(Circle [Double])` has one field of list type.
  Strictness and unpacking wrap the type: `(! String)`, `(~ T)`, and the
  `UNPACK` pragma is a form (exact shape in SPEC).
- **D1 (2026-10-02): keyword heads.** Forms with no Haskell keyword use EDN
  keyword heads: `:tuple`, `:infix`, `:as`, `:section-l`, ... A keyword
  can never be a Haskell name, so every name stays writable. The reader
  tells keywords from constructor operators: `:` followed by a letter or
  `_` is a keyword (`:tuple`, `:_`); `:` followed by symbol characters is a
  constructor operator (`:`, `:|`, `:+:`).
- **D6 (2026-10-02): tuples.** `(:tuple a b)` is `(a, b)`, as an
  expression, a pattern, and a type. In a tuple section `:_` marks a
  missing slot: `(:tuple a :_)` is `(a,)`. Unboxed tuples are
  `(:utuple a b)`. `()` is unit (expression, pattern, and type). The comma
  stays whitespace, as in EDN.
- **D7 (2026-10-02): bindings are `(= lhs rhs)` forms.** Each equation is
  its own form: `(= (f p1 p2) rhs)`, and adjacent equations for the same
  name merge into one `FunBind` exactly as GHC merges them. An infix lhs is
  `(:infix x <+> y)`. Guarded right-hand sides are clauses
  `(| qual... rhs)` (a guard may hold several qualifiers), and
  `(where binding...)` comes last. Signatures are `(:: name... type)`.
  The same shapes are used at top level and in `let`, `where`, `class` and
  `instance` bodies.
- **D9 (2026-10-02): Haskell vocabulary, Lisp shape.** Heads are Haskell
  keywords and reserved operators: `case`, `if`, `do`, `mdo`, `let`,
  `where`, `\`, `\case`, `->`, `<-`, `=>`, `::`, `=`, `|`, `data`,
  `newtype`, `type`, `class`, `instance`, `deriving`, `forall`, `import`,
  `module`, `foreign`, `pattern`, ... Contextual words (`qualified`, `as`,
  `hiding`, `family`, `stock`, `via`) keep their Haskell roles in their
  contexts. No Clojure aliases (`fn`, `defn`, ...). Examples:
  `(\ x y (+ x y))`, `(case m (-> Nothing 0) (-> (Just x) x))`,
  `(let (= n 1) (* n 2))`, `(import qualified Data.Map as M)`.
