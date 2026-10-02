#!/bin/sh
# Build a cabal-install that finds ghc-lisp (.hsl) modules: apply
# 0001-Cabal-find-ghc-lisp-hsl-modules.patch to a copy of libraries/Cabal
# (or to the Cabal checkout given as the first argument) and build the
# cabal executable with the system ghc. Prints the path of the result.
set -eu
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=${1:-$HERE/../../libraries/Cabal}
WORK=${WORK:-$(mktemp -d "${TMPDIR:-/tmp}/ghclisp-cabal.XXXXXX")}
cp -R "$SRC/." "$WORK/Cabal"
cd "$WORK/Cabal"
patch -p1 < "$HERE/0001-Cabal-find-ghc-lisp-hsl-modules.patch" >&2
cp "$HERE/cabal.ghclisp.project" .
cabal build -j exe:cabal --project-file=cabal.ghclisp.project --builddir="$WORK/dist" >&2
find "$WORK/dist" -type f -name cabal -perm -u+x | head -1
