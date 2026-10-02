# Vim and Neovim support for ghc-lisp

Filetype detection, syntax highlighting, indentation, `:make` and
conversion commands for ghc-lisp (`.hsl`) files.

## Installing

Add this directory to the runtime path, for example in `~/.vimrc`:

```vim
set runtimepath^=/path/to/ghc-lisp/ghclisp/vim
filetype plugin indent on
syntax on
```

With a plugin manager, point it at `ghclisp/vim`. In Neovim, the same
lines work in `init.vim` (or `vim.opt.runtimepath:prepend(...)` in Lua).

## What you get

- **Filetype:** `*.hsl` files get the `ghclisp` filetype.
- **Syntax:** comments and Haddock comments (`;;|`, `;;^`, `;;*`,
  `;;$name`), Haskell literals (strings with Haskell escapes, chars,
  numbers in every Haskell form, `MagicHash` suffixes), EDN keywords
  (`:tuple`, `:infix`, ...), Haskell keywords and reserved operators in
  head position (`case`, `do`, `\`, `::`, `->`, ...), constructors,
  operators, prefixes (`'Just`, `''T`, `@T`), labels and implicit
  parameters.
- **Indentation:** the layout of `ghc --hs2lisp`. Inside a list, lines are
  indented three columns past its opening parenthesis; inside a vector,
  they line up one column past its bracket. `gg=G` leaves printer output
  unchanged and indents hand-written code the same way. Indentation uses
  spaces (`expandtab`).
- **`:make`:** type-checks the current file with `ghc -fno-code` and lists
  errors and warnings at their `.hsl` positions in the quickfix list.
- **Commands:** `:GhcLispToHaskell` shows the current `.hsl` file as Haskell
  (`ghc --lisp2hs`) in a new window; `:HaskellToGhcLisp` does the reverse
  for a Haskell file (`ghc --hs2lisp`); `:GhcLispRun [args]` runs the file
  with `runghc`.

Set `g:ghclisp_ghc` (default `ghc`) and `g:ghclisp_runghc` (default
`runghc`) to use another compiler, such as the one built in this tree:

```vim
let g:ghclisp_ghc = '/path/to/ghc-lisp/_build/stage1/bin/ghc'
let g:ghclisp_runghc = '/path/to/ghc-lisp/_build/stage1/bin/runghc'
```

## Testing

`ghclisp/vim/test.sh GHC FILE.hs...` prints each file with
`ghc --hs2lisp`, opens it in Vim, and checks that it gets the `ghclisp`
filetype and that `gg=G` doesn't change the printer's layout. It is skipped
if Vim isn't installed.
