" :HaskellToGhcLisp shows the current Haskell file as ghc-lisp (ghc --hs2lisp).
command! HaskellToGhcLisp call ghclisp#Convert('--hs2lisp', 'ghclisp')
