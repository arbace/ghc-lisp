" Conversions between Haskell and ghc-lisp with ghc --hs2lisp / --lisp2hs.

function! ghclisp#Convert(flag, filetype) abort
  let ghc = get(g:, 'ghclisp_ghc', 'ghc')
  if &modified
    write
  endif
  let out = systemlist(ghc . ' ' . a:flag . ' ' . shellescape(expand('%')))
  if v:shell_error
    echohl ErrorMsg | echo join(out, "\n") | echohl None
    return
  endif
  new
  setlocal buftype=nofile bufhidden=wipe noswapfile
  call setline(1, out)
  execute 'setfiletype ' . a:filetype
endfunction
