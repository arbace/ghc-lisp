# ghc-lisp: the s-expression front end (ghclisp/DESIGN.md).

# Compile and run a whole program written in ghc-lisp.
test('lisp001', [], multimod_compile_and_run, ['lisp001.hsl', ''])

# --make with Haskell and ghc-lisp modules importing each other.
test('lisp002', [extra_files(['Geo/'])],
     multimod_compile_and_run, ['lisp002.hs', ''])

# Type errors are reported at positions in the .hsl source.
test('lisp003', [], multimod_compile_fail, ['lisp003.hsl', ''])

# CPP is rejected in .hsl files.
test('lisp004', [], multimod_compile_fail, ['lisp004.hsl', ''])

# Lexemes that depend on the file's extensions.
test('lisp005', [], multimod_compile_and_run, ['lisp005.hsl', ''])

# ghc --hs2lisp prints a Haskell module as ghc-lisp.
test('lisp006', [], run_command, ['{compiler} --hs2lisp lisp006.hs'])

# ghc --lisp2hs prints a ghc-lisp module as Haskell.
test('lisp007', [], run_command, ['{compiler} --lisp2hs lisp007.hsl'])

# ghc --lisp-check: Haskell -> Lisp -> Haskell reproduces the tree.
test('lisp008', [extra_files(['lisp006.hs'])], run_command,
     ['{compiler} --lisp-check lisp006.hs'])

# GHCi loads .hsl modules.
test('lisp009', [extra_files(['lisp009.hsl'])], ghci_script, ['lisp009.script'])

# Vim support (ghclisp/vim): gg=G keeps the printer's layout, and tokens are
# highlighted. Prints nothing if all is well or if vim isn't installed.
test('lisp010', [extra_files(['lisp006.hs', 'lisp001.hsl'])], run_command,
     ['QUIET=1 sh {top}/../ghclisp/vim/test.sh {compiler} lisp006.hs'])
