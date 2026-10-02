" Vim compiler file for ghc-lisp: :make type-checks the current file with
" ghc and lists errors and warnings at their .hsl positions.

if exists('current_compiler')
  finish
endif
let current_compiler = 'ghclisp'

let s:cpo_save = &cpo
set cpo&vim

execute 'CompilerSet makeprg=' . escape(get(g:, 'ghclisp_ghc', 'ghc') . ' -fno-code %', ' ')
" GHC reports  file:line:col: error: ...  (or a line:col-col range), and
" continues the message on indented lines.
CompilerSet errorformat=
      \%E%f:%l:%c:\ error:%m,
      \%W%f:%l:%c:\ warning:%m,
      \%E%f:%l:%c-%*\\d:\ error:%m,
      \%W%f:%l:%c-%*\\d:\ warning:%m,
      \%C\ \ \ \ %m,
      \%-G%.%#

let &cpo = s:cpo_save
unlet s:cpo_save
