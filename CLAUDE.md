# ghc-lisp

This is a fork of GHC that adds a second front end to the compiler. It
accepts GHC Haskell written as Clojure/EDN-style s-expressions, in `.hsl`
files. The design lives in `ghclisp/DESIGN.md`. It is still being worked out
with the user, one iteration at a time. Read it first, and ask the user
before settling any open decision (D1, D2, ...). When a decision is settled,
record it in the "Decided" section of that file. The sister project is
go-lisp (github.com/arbace/go-lisp), which did the same for Go.

## Git rules (hard)

- Commit and push **only** on the `ghc-lisp` branch.
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
- First build: `./boot && ./configure && hadrian/build -j --flavour=quick`.
  The resulting compiler is `_build/stage1/bin/ghc`.
- Fast rebuild loop after compiler changes: TBD (to be filled in once the
  first change has been built).
