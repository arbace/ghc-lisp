# cabal support for ghc-lisp

Cabal finds a module's source by trying a fixed list of suffixes, and
checks that `main-is` names a Haskell (`.hs`, `.lhs`) or C file. Cabal is
a separate project (`libraries/Cabal` is a submodule), so the change that
adds `.hsl` lives here as a patch:
`0001-Cabal-find-ghc-lisp-hsl-modules.patch` adds `"hsl"` to
`builtinHaskellSuffixes` (module search for building, `sdist`, and
change detection in cabal-install) and `.hsl` to the checks of `main-is`
and of test and benchmark main files. Each change is one line, marked
`ghc-lisp:`. The patch applies to the Cabal 3.16 branch in
`libraries/Cabal`.

## Building cabal-install with the patch

```sh
ghclisp/cabal/build-cabal.sh        # prints the path of the new cabal
```

The script copies `libraries/Cabal`, applies the patch and builds
`exe:cabal` with the system `ghc`, using `cabal.ghclisp.project` (only the
four packages cabal-install needs, without test dependencies). It takes
about six minutes.

## Using it

Modules and `main-is` files can be `.hsl` files; nothing else changes:

```cabal
library
  exposed-modules: Geo.Shape Geo.Util   -- src/Geo/Shape.hsl, src/Geo/Util.hs
  hs-source-dirs:  src

executable geo
  main-is:         Main.hsl
```

Build with the ghc-lisp compiler:

```sh
cabal build -w /path/to/ghc-lisp/_build/stage1/bin/ghc \
            --with-hc-pkg=/path/to/ghc-lisp/_build/stage1/bin/ghc-pkg
```

`cabal build`, `cabal run`, `cabal sdist` (which includes the `.hsl`
files) and rebuilds after editing a `.hsl` file work.
