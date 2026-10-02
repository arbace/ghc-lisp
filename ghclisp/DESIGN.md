# ghc-lisp: design notes (living document)

Status: **draft v1**. The syntax is not settled. Every open decision has an
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

## 4. Syntax sketch (v0, every line is up for discussion)

```clojure
(:language LambdaCase ScopedTypeVariables)      ; {-# LANGUAGE ... #-}   (D10)

(module Data.Shape [Shape (..) area mk-square]  ; module Data.Shape (Shape(..), area, mkSquare) where
                                                ; ^ names: D3
  (import Data.List [sort-by])                  ; import Data.List (sortBy)
  (import qualified Data.Map.Strict as M)       ; Haskell's own words (D9)
  (import Prelude hiding [lookup])

  (data Shape                                   ; data Shape = Circle Double | Rect { w, h :: Double }
    (Circle Double)
    (Rect [w h Double])
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
    (do (<- xs (fmap lines get-contents))       ; xs <- lines <$> getContents
        (let (= n (length xs)))
        (print (:tuple n (sort-by (comparing negate) [1 2 3])))   ; (D5, D6)
        (mapM_ (\case (-> 0 (pure ())) (-> k (print k))) [n])
        (print (:infix 1 + 2 * 3)))))           ; mixed chain, fixity left to the renamer (D4)
```

## 5. Open decisions

**D1. Heads for forms with no Haskell keyword.** Carry over go-lisp D1:
EDN keyword heads (`:tuple`, `:infix`, `:list`, `:as`, ...). Haskell
wrinkle: constructor operators start with `:` (`:`, `:|`, `:+:`). The
reader can tell them apart: a keyword is `:` followed by a letter or `_`;
`:` followed by symbol characters is a constructor operator. Recommended.

**D2. Lexemes.** EDN structure (lists, vectors, `#_`; `{}`, `#{}`, `#tag`
reserved) plus Haskell's own literal grammar: strings with Haskell escapes
(`\&`, `\SOH`, `\^A`, string gaps), char literals `'a'`, numbers with
`NumericUnderscores`, hex floats, binary literals, `MagicHash` suffixes
(`3#`, `3##`, `"x"#`). Identifiers may contain `'` (`foldl'`, `x'`), which
the reader has to tell apart from char literals and from promotion /
TH name quotes (`'Just`, `''T`). Multiline strings (`"""`) maybe later.

**D3. Names.** Haskell gets meaning from case (constructors and types
uppercase, variables lowercase), and exports are explicit lists, so
go-lisp's "exported by default" (its D18) doesn't apply. Options:
  - (a) **verbatim**: names are Haskell names (`sortBy`, `foldl'`, `M.insert`).
    Simplest; the mapping is the identity.
  - (b) **kebab-case** for lowercase names: `sort-by` -> `sortBy`, with a
    rule for names that already contain capitals, like go-lisp's.
    Lisp-looking, but needs exemptions and escape rules, and error messages
    show Haskell names.

**D4. Operators and fixity.** GHC's parser doesn't know fixities: it builds
operator chains flat and the renamer re-associates them, unless a node is
wrapped in `HsPar`. Proposal:
  - `(op a b c ...)` is the Haskell chain `a op b op c` — unresolved, so the
    renamer applies the real fixity (`(- a b c)` is `(a - b) - c`, `(++ a b c)`
    is `a ++ (b ++ c)`, `(== a b c)` is a fixity error, as in Haskell).
  - `(:infix a + b * c)` is a mixed chain, exactly Haskell's `a + b * c`.
    Operators in a chain are ordinary or backtick names (`(:infix a div b)`).
  - A nested operator form gets `HsPar`, so the Lisp grouping always wins:
    `(* (+ a b) c)` is `(a + b) * c`.
  - `(- x)` with one argument is negation (`NegApp`).
  - Sections: `(:section-l a +)` / `(:section-r + b)`? `(+ 1)` is a plain
    partial application, which is a different AST from a section.
  - The operator as a value is just the symbol: `(foldr + 0 xs)`.

**D5. What are vectors?** Haskell has list syntax everywhere: list
literals, list patterns, the list type `[a]`. Options:
  - (a) **vectors are Haskell brackets**: `[1 2 3]`, pattern `[x y]`, type
    `[Int]`. Syntax-only groupings (contexts, export lists, binders) use
    lists or vectors positionally.
  - (b) go-lisp's rule, **vectors are never expressions**: list literals are
    `(:list 1 2 3)`, the list type `(:list-of Int)`, and vectors are kept
    for syntax (contexts, import lists, record fields, binders).

**D6. Tuples and unit.** `,` is whitespace in EDN. `(:tuple a b)`,
`(:tuple a :_)` for tuple sections, `(:utuple a b)` for `(# a, b #)`, and
`()` for unit? Or keep `,` as a symbol: `(, a b)`.

**D7. Bindings.** Function equations are separate `(= lhs rhs)` forms,
merged into one `FunBind` exactly as GHC merges adjacent equations. The
lhs is `(f p1 p2)`, or `(:infix x <+> y)` for an infix definition. Guards
are `(| guard... rhs)` clauses, and `(where binding...)` goes last.
Signatures are `(:: name... type)`. The same shapes appear at top level, in
`let`, `where`, `class` and `instance` bodies.

**D8. Layout.** S-expressions replace layout: `do`, `let`, `where`, `case`,
`class`, `instance`, `\case` bodies are the form's remaining elements.
No braces, no semicolons.

**D9. Haskell vocabulary, Lisp shape.** Heads are Haskell keywords and
reserved operators: `case`, `of`?, `if`, `do`, `mdo`, `let`, `where`, `\`,
`\case`, `->`, `<-`, `=>`, `::`, `=`, `|`, `@`, `~`, `!`, `data`,
`newtype`, `type`, `class`, `instance`, `deriving`, `forall`, `import`,
`module`, `foreign`, `pattern`, ... No Clojure aliases (`defn`, `fn`, ...).
Contextual words (`qualified`, `as`, `hiding`, `family`, `stock`, `via`)
stay contextual.

**D10. Pragmas.** File-header pragmas (`LANGUAGE`, `OPTIONS_GHC`) must be
readable by the downsweep before the full parse. Options:
  - (a) forms before `module`: `(:language GADTs)`, `(:options-ghc "-Wall")`.
  - (b) directive comments like go-lisp D7: `;#language GADTs`.

  Pragmas inside the code (`INLINE`, `SPECIALISE`, `RULES`, `UNPACK`,
  `SCC`, `COMPLETE`, `MINIMAL`, `OVERLAPPING`, ...) are AST nodes, so they
  are forms either way.

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
