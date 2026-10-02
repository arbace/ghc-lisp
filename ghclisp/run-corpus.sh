#!/bin/sh
# Behavioral corpus test for ghc-lisp (the counterpart of go-lisp's
# TestLispRunCorpus): for each single-module Haskell program, convert it to
# .hsl with ghc --hs2lisp, compile both versions, run both, and compare the
# output and the exit code.
#
# Usage: ghclisp/run-corpus.sh GHC [FILE...]
# Without files, uses the testsuite's should_run programs. Programs whose
# Haskell version doesn't compile or run on its own (extra modules, flags,
# input, arguments) are skipped. JOBS sets the parallelism (default 32).

set -u

if [ "${1:-}" = "--one" ]; then
  GHC=$2 WORK=$3 f=$4
  d=$(mktemp -d "$WORK/t.XXXXXX")
  cp "$f" "$d/Main.hs"
  cd "$d" || exit 1
  if ! timeout 120 "$GHC" -O0 -v0 Main.hs -o prog >/dev/null 2>&1; then echo "SKIP $f"; exit 0; fi
  timeout 30 ./prog </dev/null >hs.out 2>hs.err; hs_rc=$?
  if [ $hs_rc -ge 124 ]; then echo "SKIP $f"; exit 0; fi
  mkdir lisp
  if ! timeout 60 "$GHC" --hs2lisp Main.hs >lisp/Main.hsl 2>conv.err; then
    echo "FAIL $f: hs2lisp: $(head -c 200 conv.err | tr '\n' ' ')"; exit 0
  fi
  cd lisp || exit 1
  if ! timeout 120 "$GHC" -O0 -v0 Main.hsl -o prog >build.out 2>&1; then
    echo "FAIL $f: compile: $(head -c 300 build.out | tr '\n' ' ')"; exit 0
  fi
  timeout 30 ./prog </dev/null >hsl.out 2>hsl.err; hsl_rc=$?
  if [ $hs_rc -ne $hsl_rc ]; then echo "FAIL $f: exit code $hs_rc vs $hsl_rc"; exit 0; fi
  if ! cmp -s ../hs.out hsl.out; then echo "FAIL $f: stdout differs"; exit 0; fi
  echo "OK $f"
  cd / && rm -rf "$d"
  exit 0
fi

GHC=${1:?usage: run-corpus.sh GHC [FILE...]}
shift
GHC=$(cd "$(dirname "$GHC")" && pwd)/$(basename "$GHC")
ROOT=$(cd "$(dirname "$0")/.." && pwd)
SELF=$ROOT/ghclisp/run-corpus.sh
WORK=$(mktemp -d "${TMPDIR:-/tmp}/ghclisp-run.XXXXXX")

# Programs whose output legitimately differs (as go-lisp's F12): they print
# their own source locations (call stacks, static pointers, cost centres,
# type errors), or their output depends on timing.
EXCLUDE='
deSugar/should_run/DsStaticPointers.hs
deSugar/should_run/T19289.hs
profiling/should_run/T25675.hs
typecheck/should_run/IPLocation.hs
typecheck/should_run/T10284.hs
typecheck/should_run/T10845.hs
typecheck/should_run/T10846.hs
typecheck/should_run/T11049.hs
typecheck/should_run/T22086.hs
typecheck/should_run/T25529.hs
concurrent/should_run/conc065.hs
concurrent/should_run/conc066.hs
'

if [ $# -eq 0 ]; then
  find "$ROOT/testsuite/tests" -path '*should_run*' -name '*.hs' \
    | xargs grep -l '^main\b' | sort \
    | grep -v -F "$(echo "$EXCLUDE" | sed '/^$/d')" > "$WORK/files"
else
  for f in "$@"; do echo "$f"; done > "$WORK/files"
fi

xargs -P "${JOBS:-32}" -I{} "$SELF" --one "$GHC" "$WORK" {} < "$WORK/files" > "$WORK/results"

echo "OK: $(grep -c '^OK' "$WORK/results")  FAIL: $(grep -c '^FAIL' "$WORK/results")  SKIP: $(grep -c '^SKIP' "$WORK/results")"
grep '^FAIL' "$WORK/results" | sort
echo "results: $WORK/results"
