# ghc-lisp

This is a fork of GHC that adds a second front end to the compiler. It
accepts GHC Haskell written as Clojure/EDN-style s-expressions, in `.hsl`
files. The design lives in `ghclisp/DESIGN.md`. The normative per-form grammar is
`ghclisp/SPEC.md`. Read both first. This is a pathfinder implementation:
when a new design choice comes up, take the recommended option, record it
(DESIGN.md "Decided" log or SPEC.md §12) and keep going; the user reviews
the log rather than each choice. The sister project is
go-lisp (github.com/arbace/go-lisp), which did the same for Go.

## Git rules (hard)

- Commit and push **only** on the `ghc-lisp` branch. It is the default branch
  of `origin` (github.com/arbace/ghc-lisp).
- **Never** commit to, push to, or merge into `master`. `master` mirrors
  upstream GHC.
- Commit messages follow GHC style: a short summary line, a blank line,
  then the explanation. Use the prefix `ghclisp:` for docs and tooling in
  this repo.

## Keep upstream files untouched

The goal is painless merges from upstream GHC.
- Put new code in new modules and directories: `compiler/GHC/Parser/Lisp/`
  for the reader, parser and printer, `utils/check-lisp` for the round-trip
  harness, and `ghclisp/` for docs and specs.
- If an existing upstream file must change, keep the edit as small as
  possible (a one-line hook) and mark it with a `ghc-lisp:` comment
  (`-- ghc-lisp: ...` in Haskell and cabal files), so `git grep 'ghc-lisp:'`
  lists every intrusion.
- Don't reformat or refactor upstream code.

## Architecture in brief

- The Lisp parser produces the same `HsModule GhcPs` as `GHC.Parser`. The
  renamer and everything after it stay unchanged.
- Compiler entry point: the parser choice in `hscParse'`
  (`compiler/GHC/Driver/Main/Passes.hs`). The downsweep reads module
  headers separately (`compiler/GHC/Parser/Header.hs`), and the finder and
  phases need to know the `.hsl` suffix.
- The Haskell -> Lisp -> Haskell mapping must be lossless. Correctness is
  tested by round-tripping the ASTs of the tree's own Haskell sources, as
  `utils/check-ppr` does for GHC's pretty-printer.

## Building and testing

- Bootstrap: the system `ghc` (9.14.1), `cabal`, `alex`, and `happy` 2.x
  from `/root/.local/bin` (Alpine's happy 1.21 is rejected by configure),
  so put `/root/.local/bin` first on `PATH`.
- First build: `./boot && ./configure && hadrian/build -j --flavour=quick`
  (about 30 minutes). The compiler is `_build/stage1/bin/ghc`.
- After changing the compiler: `hadrian/build -j --flavour=quick --freeze1
  _build/stage1/bin/ghc` (about a minute; the stage-1 compiler and the
  libraries are not rebuilt).
- Round trip: `_build/stage1/bin/ghc --lisp-check FILE.hs...` prints OK or
  FAIL per file; with `GHC_LISP_CHECK_OUT=DIR` it writes the Lisp text and
  both AST dumps of failing files there. Run it over the corpus in
  parallel, e.g. `find compiler libraries utils -name '*.hs' | xargs -P 60
  -n 20 _build/stage1/bin/ghc --lisp-check | grep '^FAIL'`. Files reported
  as ERROR don't parse as Haskell in isolation and are skipped.
- Behavioral test: `ghclisp/run-corpus.sh _build/stage1/bin/ghc` (about
  10 minutes).
- Testsuite: `hadrian/build -j --flavour=quick --freeze1 test
  --test-root-dirs=testsuite/tests/ghclisp` (11 tests).
- Haddock: `ghc --lisp-check -haddock FILE.hs` also compares the attached
  documentation (about 86% of source files today; see DESIGN.md).
- Vim: `ghclisp/vim/test.sh _build/stage1/bin/ghc FILE.hs...` checks that
  `gg=G` keeps the printer's layout. If you change the printer's layout,
  keep `ghclisp/vim/indent/ghclisp.vim` in step (lists indent three columns
  past `(`, vectors one past `[`).
- cabal: `ghclisp/cabal/build-cabal.sh` builds a cabal-install that finds
  `.hsl` modules (a patch to the Cabal submodule; don't commit inside
  `libraries/Cabal`).
- Convert: `ghc --hs2lisp X.hs`, `ghc --lisp2hs X.hsl`. Compile `.hsl`
  files like `.hs` files. User guide: `ghclisp/README.md`.
- Don't run `_build/stage1/bin/ghc` while hadrian is relinking it (bus
  errors).
