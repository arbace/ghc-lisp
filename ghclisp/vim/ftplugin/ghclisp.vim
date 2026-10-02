" Vim filetype plugin for ghc-lisp (.hsl).

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal comments=:;;,:;
setlocal commentstring=;;\ %s
setlocal iskeyword=@,48-57,_,',-,#,?,:,.
setlocal formatoptions-=t formatoptions+=croql
setlocal expandtab shiftwidth=2 softtabstop=2
compiler ghclisp

let b:undo_ftplugin = 'setlocal comments< commentstring< iskeyword< formatoptions< expandtab< shiftwidth< softtabstop<'
      \ . ' | delcommand -buffer GhcLispToHaskell | delcommand -buffer GhcLispRun'

let s:ghc = get(g:, 'ghclisp_ghc', 'ghc')

" :GhcLispToHaskell shows this buffer's file as Haskell (ghc --lisp2hs).
command! -buffer GhcLispToHaskell call ghclisp#Convert('--lisp2hs', 'haskell')
" :GhcLispRun runs this buffer's file (runghc).
command! -buffer -nargs=* GhcLispRun execute '!' . get(g:, 'ghclisp_runghc', 'runghc') . ' ' . shellescape(expand('%')) . ' <args>'
