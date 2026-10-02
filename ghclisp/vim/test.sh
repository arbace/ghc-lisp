#!/bin/sh
# Test the Vim support: for each Haskell file, print it with ghc --hs2lisp,
# then check that Vim's gg=G leaves the printer's layout unchanged and that
# the file gets the ghclisp filetype and syntax.
# It also checks the highlighting of a few kinds of tokens.
# Usage: ghclisp/vim/test.sh GHC FILE.hs...   (skipped if vim is missing)
# With QUIET=1, only failures are printed.

set -u
GHC=$1
shift
VIM=${VIM:-vim}
QUIET=${QUIET:-}
say() { [ -n "$QUIET" ] || echo "$@"; }
if ! command -v "$VIM" >/dev/null 2>&1; then say "SKIP: no vim"; exit 0; fi
RTP=$(cd "$(dirname "$0")" && pwd)
WORK=$(mktemp -d "${TMPDIR:-/tmp}/ghclisp-vim.XXXXXX")
fail=0
for f in "$@"; do
  base=$WORK/$(basename "$f" .hs)
  "$GHC" --hs2lisp "$f" > "$base.hsl" 2>/dev/null || { say "SKIP $f"; continue; }
  cp "$base.hsl" "$base.orig"
  "$VIM" -Es -u NONE -i NONE -N --cmd "set rtp^=$RTP" \
    --cmd 'filetype plugin indent on' --cmd 'syntax on' \
    -c 'if &filetype !=# "ghclisp" | cquit | endif' \
    -c 'normal! gg=G' -c 'wq' "$base.hsl" || { echo "FAIL $f: vim"; fail=1; continue; }
  if cmp -s "$base.orig" "$base.hsl"; then
    say "OK $f"
  else
    echo "FAIL $f: gg=G changed the layout"
    diff "$base.orig" "$base.hsl" | head -10
    fail=1
  fi
done

# Highlighting: the syntax group at the start of each marked token.
cat > "$WORK/hl.hsl" <<'EOF'
;;| A doc comment
(module Main [main])
(= main (print (:tuple "str" 'c' 0x1F (Just 1))))
EOF
"$VIM" -Es -u NONE -i NONE -N --cmd "set rtp^=$RTP" \
  --cmd 'filetype plugin indent on' --cmd 'syntax on' \
  -c "redir! > $WORK/hl.out" \
  -c 'for [l, c] in [[1, 1], [2, 2], [3, 2], [3, 17], [3, 24], [3, 30], [3, 34], [3, 40]] | echo synIDattr(synID(l, c, 1), "name") | endfor' \
  -c 'redir END' -c 'q!' "$WORK/hl.hsl"
expected="ghclispDocComment ghclispSpecial ghclispSpecial ghclispKeyword ghclispString ghclispChar ghclispNumber ghclispConstructor"
got=$(tr -s '\n' ' ' < "$WORK/hl.out" | sed 's/^ *//; s/ *$//')
if [ "$got" = "$expected" ]; then
  say "OK highlighting"
else
  echo "FAIL highlighting: got: $got"
  fail=1
fi
exit $fail
