# ghc-lisp

**GHC that also compiles Haskell written as s-expressions.**

ghc-lisp is a fork of the [Glasgow Haskell Compiler](https://gitlab.haskell.org/ghc/ghc)
with a second front end. Next to `.hs` files it compiles `.hsl` files:
the whole of GHC Haskell, written in a Clojure/EDN-flavored Lisp syntax.

```clojure
(module Main [main])

(import qualified Data.Map.Strict as M)

(:: wordCounts (-> String (M.Map String Int)))
(= (wordCounts text)
   (M.fromListWith + [(:tuple w 1) | (<- w (words text))]))

(:: main (IO ()))
(= main
   (do (<- text getContents)
       (mapM_ print (M.toList (wordCounts text)))))
```

A `.hsl` module is parsed into exactly the syntax tree that GHC's own
parser builds for the equivalent Haskell. So everything after parsing
(the type checker, optimiser, code generator and GHCi) is unchanged, and
`.hs` and `.hsl` modules import each other freely. It isn't a new
language, and there are no macros: just another way to write Haskell.

## What works

- `ghc Foo.hsl`, `ghc --make` with mixed `.hs`/`.hsl` modules, GHCi,
  `runghc`; errors point into the `.hsl` source
- `ghc --hs2lisp` and `ghc --lisp2hs` convert in both directions
  (comments and Haddock docs included)
- Every Haskell file in GHC's own source tree converts to `.hsl` and back
  to the identical syntax tree; 1,069 of GHC's test programs behave the
  same when compiled from `.hsl`
- Vim/Neovim support, and cabal support through a small patch

Start with the **[user guide and cheat sheet](../ghclisp/README.md)**. The
design is in [DESIGN.md](../ghclisp/DESIGN.md) and the full grammar in
[SPEC.md](../ghclisp/SPEC.md).

## Relation to GHC

- The default branch, `ghc-lisp`, is upstream GHC plus this front end.
  `master` mirrors upstream GHC unchanged.
- The new code lives in new files (`compiler/GHC/Parser/Lisp*`,
  `ghclisp/`). Upstream files have only a handful of one-line hooks, each
  marked `ghc-lisp:` (`git grep 'ghc-lisp:'`), so upstream merges stay easy.
- This is an independent experiment, not part of the GHC project. Please
  report bugs in GHC itself
  [upstream](https://gitlab.haskell.org/ghc/ghc/-/issues), not here.
  GHC's own README is [README.md](../README.md).

Sister project: [go-lisp](https://github.com/arbace/go-lisp), the same
idea for the Go compiler.

## Building

Build like GHC (`./boot && ./configure && hadrian/build`); see
[CLAUDE.md](../CLAUDE.md) for the exact commands, and
[ghclisp/README.md](../ghclisp/README.md) for using the result.
