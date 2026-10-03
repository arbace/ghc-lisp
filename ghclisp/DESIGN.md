# ghc-lisp: design notes (living document)

Status: **draft v4**. The syntax is not settled. Every open decision has an
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

None of the syntax-level decisions (D1-D16) are open; see the log below.
Detailed shapes (every `GhcPs` constructor, its disambiguation rule and its
round-trip comparison) go into `ghclisp/SPEC.md`, written next.

## 6. Roadmap

0. Settle D1..D16 with worked examples (done). Write `ghclisp/SPEC.md` with one
   form per AST constructor.
1. Reader with positions, plus tests.
2. Printer (AST -> Lisp) first: it forces a complete mapping for every
   constructor and generates the corpus.
3. Parser (Lisp -> AST), plus `utils/check-lisp` and the corpus round trip.
4. Driver hooks: `ghc Foo.hsl`, hello world, `--make` with mixed `.hs` /
   `.hsl` modules. Then a behavioral test: testsuite run-tests converted to
   `.hsl` behave identically.
5. Tooling: `hs2lisp` / `lisp2hs`, error messages, editor mode, cabal.

## Status (2026-10-02)

| Roadmap step | State |
|---|---|
| 1. Reader | done: `GHC/Parser/Lisp/Reader.hs`. EDN structure; every atom is lexed by GHC's own lexer with the file's extensions. |
| 2. Printer (Haskell -> ghc-lisp) | done: `GHC/Parser/Lisp/Printer.hs`, `ghc --hs2lisp`. |
| 3. Parser (ghc-lisp -> AST) | done: `GHC/Parser/Lisp.hs`, reusing `GHC.Parser.PostProcess`. The round trip (`ghc --lisp-check`) is exact for all 4,863 parseable Haskell files in `compiler/`, `libraries/`, `utils/`, `ghc/` and `hadrian/`, and for 11,382 of the 11,423 parseable files in `testsuite/tests` (99.6%). Files that GHC itself can't parse in isolation (CPP headers from build directories, default extensions from .cabal files, expected-failure tests) are skipped. |
| 4. Compiler hooks | done: `ghc Foo.hsl`, `ghc --make` with mixed `.hs`/`.hsl` modules, GHCi `:load`, `runghc`, `-fhpc`. Diagnostics point into the `.hsl` source. Behavioral test: `ghclisp/run-corpus.sh` converts the testsuite's single-module should_run programs to `.hsl`; 1,069 behave identically to the Haskell originals (12 excluded: they print their own source locations or depend on timing). |
| 5. Build tools | `ghc --make` and GHCi build mixed `.hs`/`.hsl` programs. cabal: `ghclisp/cabal` holds a one-line-per-check patch to Cabal (a submodule) and a script that builds cabal-install with it; `cabal build`, `run` and `sdist` then work on packages with `.hsl` modules. |
| 6. Tooling | `ghc --hs2lisp` (keeps comments; Haddock comments become `;;|`, `;;^`, ...), `ghc --lisp2hs` (GHC's pretty-printer), `ghc --lisp-check`; `;;|` comments reach `-haddock` through GHC's own Haddock pass (with `-haddock`, 4,185 of 4,863 source files round-trip including their docs). Vim and Neovim: `ghclisp/vim`. Tests in `testsuite/tests/ghclisp`. |

Upstream hooks, all marked (`git grep 'ghc-lisp:'`): the module list in
`compiler/ghc.cabal.in`; `GHC.Driver.Phases` (the `.hsl` and `.hsl-boot` suffixes),
`GHC.Unit.Finder` (search `.hsl`), `GHC.Parser.Header` (header parse and
file options), `GHC.Driver.Main.Passes` (module parse); the mode flags and
dispatch in `ghc/` (`--hs2lisp`, `--lisp2hs`, `--lisp-check`).

### Known limits

- `hs2lisp` puts a comment that was inside a line of code in front of the
  next printed line; with `-haddock`, some doc comments attach differently
  than in the Haskell original (14% of source files differ in their docs, mostly comments after the last item of a list).
- Diagnostics print expressions in Haskell syntax (names are the same, D3).
- cabal needs the patch in `ghclisp/cabal`.

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
- **D2 (2026-10-02): EDN structure, Haskell lexemes.** Lists, vectors and
  `#_` as in EDN; `{}`, `#{}` and `#tag` are read but reserved. Literals
  follow Haskell's lexical grammar exactly: strings with Haskell escapes
  (`\&`, `\SOH`, `\^A`) and gaps, char literals `'a'`, numbers with
  `NumericUnderscores`, hex floats, binary literals, and `MagicHash`
  suffixes (`3#`, `3##`, `"x"#`). The literal's source text is kept
  verbatim, as GHC keeps it. `'` follows GHC's lexer: inside a name it is a
  prime (`foldl'`, `x''`), `'a'` is a char literal, and `'Just` / `''T`
  are promotion ticks and TH name quotes. A leading sign (`-5`) reads as
  `(- 5)`, a negation, as in Haskell (`NegativeLiterals` details: SPEC).
  The comma is whitespace. Multiline strings (`"""`) may come later.
- **D8 (2026-10-02): no layout.** A block's statements, bindings or
  alternatives are the form's remaining elements, with no wrapper:
  `(do (<- x get) (print x))`, `(let (= a 1) (= b 2) (+ a b))` (the last
  element is the body), `(case m (-> Nothing 0) (-> (Just x) x))`,
  `(class (Show a) (:: show (-> a String)))`.
- **D11 (2026-10-02): no CPP in `.hsl` files for now.** Enabling `CPP` in
  a `.hsl` file is an error. Converting a CPP'd `.hs` file captures one
  configuration; the corpus test works on the post-CPP AST. Conditional
  compilation may get its own form later.
- **D12 (2026-10-02): Haddock comments mirror Haddock.** `;;|` documents
  what follows (`-- |`), `;;^` what precedes (`-- ^`), and `;;*` starts a
  section heading (`-- *`). With `-haddock` they go into the AST as
  Haddock comments do. Plain `;` comments are ignored by the parser.
  `hs2lisp` keeps comments where it can, but the round trip doesn't
  compare them.
- **D13 (2026-10-02): spans only.** Every node gets a real `SrcSpan` from
  its form, so diagnostics, HIE files and the debugger point into the
  `.hsl` file. Exact-print annotations stay empty (`noAnn`), except where a
  later pass turns out to read them. `lisp2hs` prints with GHC's `ppr`, not
  exact print. `ghc-exactprint` doesn't support `.hsl` ASTs. The round trip
  compares with `BlankSrcSpan` / `BlankEpAnnotations`, as check-ppr does.
- **D14 (2026-10-02): milestone 1 is all of GHC's syntax, printer first.**
  As in go-lisp, the printer maps every `GhcPs` constructor, and the corpus
  defines done: the Haskell sources in `libraries/`, `compiler/`, `utils/`
  and the `testsuite/tests` files that parse. Every round-trip failure is a
  bug. Template Haskell, arrows, linear types and the rest are in scope,
  ordered by how often the corpus uses them. Progress is measured as files
  passing out of files total.
- **D15 (2026-10-02): dots mean qualification only.** A dotted symbol is a
  qualified name: uppercase module segments, then the name (`M.insert`,
  `Data.Map.Map`, `M.!`, `Prelude..`). `.` alone is composition:
  `(. show length)`. `OverloadedRecordDot` is always a form:
  `(:get r name)` is `r.name`, `(:get (f x) a b)` is `(f x).a.b`, and
  `(:proj name)` is the projection section `(.name)`.
- **D16 (2026-10-02): Template Haskell uses keyword forms.** `(:splice e)`
  is `$e`/`$(e)`, `(:typed-splice e)` is `$$e`. Quotes: `(:quote e)`
  (`[| e |]`), `(:quote-type T)`, `(:quote-pat p)`, `(:quote-decls d...)`,
  `(:typed-quote e)` (`[|| e ||]`). Name quotes `'f` and `''T` are
  lexemes (D2). A quasiquote is `(:qq quoter "text")`; its body stays text,
  in Haskell string syntax. No `$x` shorthand: `$` is an ordinary operator.
