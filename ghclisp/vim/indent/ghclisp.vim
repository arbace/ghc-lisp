" Vim indent file for ghc-lisp (.hsl). It reproduces the layout of
" ghc --hs2lisp: inside a list, lines are indented three columns past its
" opening parenthesis (the head, then two); inside a vector, they line up
" one column past its opening bracket. So gg=G leaves printer output as it
" is, and indents hand-written code the same way.

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal indentexpr=GhcLispIndent()
setlocal indentkeys=!^F,o,O,0),0]
setlocal nolisp
setlocal autoindent

let b:undo_indent = 'setlocal indentexpr< indentkeys< lisp< autoindent<'

if exists('*GhcLispIndent')
  finish
endif

" Characters that matter for nesting: comments, strings, chars, brackets.
let s:interesting = '[;"''()[\]]'
" The rest of a string literal after its opening quote, up to the closing one.
let s:string_rest = '^\%(\\.\|[^"\\]\)*"'
" A char literal: 'x' or an escape such as '\n' or '\''.
let s:char = '^''\%(\\[^'']\+\|\\''\|[^\\'']\)'''

" Scan one line, updating the stack of unclosed ( and [ ([line, col, char]),
" skipping strings, chars and comments. A string may continue on the next
" line (a Haskell string gap); the result says whether the line ends inside
" one. It jumps from one interesting character to the next with match(),
" which is much faster than looking at every character in Vim script.
function! s:ScanLine(stack, l, in_string) abort
  let line = getline(a:l)
  if a:in_string
    let i = matchend(line, s:string_rest, 0) - 1
    if i < 0
      return 1
    endif
    let i = match(line, s:interesting, i + 1)
  else
    let i = match(line, s:interesting)
  endif
  while i >= 0
    let c = line[i]
    if c ==# ';'
      break
    elseif c ==# '"'
      let i = matchend(line, s:string_rest, i + 1) - 1
      if i < 0
        return 1
      endif
    elseif c ==# "'"
      " a char literal; otherwise a prime or a prefix tick
      let e = matchend(line, s:char, i)
      if e > 0
        let i = e - 1
      endif
    elseif c ==# '(' || c ==# '['
      call add(a:stack, [a:l, i, c])
    elseif !empty(a:stack)
      call remove(a:stack, -1)
    endif
    let i = match(line, s:interesting, i + 1)
  endwhile
  return 0
endfunction

" The state before line lnum: the stack, and whether the line starts inside
" a string. Indenting a range asks for consecutive lines, so the state
" before the previous line is kept and only that line, which has just been
" reindented, is scanned again.
function! s:StateBefore(lnum) abort
  let st = get(b:, 'ghclisp_indent_state', {})
  if get(st, 'lnum', -1) == a:lnum - 1
    let stack = st.stack
    let in_string = s:ScanLine(stack, a:lnum - 1, st.in_string)
  else
    let stack = []
    let in_string = 0
    for l in range(1, a:lnum - 1)
      let in_string = s:ScanLine(stack, l, in_string)
    endfor
  endif
  let b:ghclisp_indent_state = {'lnum': a:lnum, 'stack': copy(stack), 'in_string': in_string}
  return [stack, in_string]
endfunction

function! GhcLispIndent() abort
  let [stack, in_string] = s:StateBefore(v:lnum)
  if in_string
    " inside a multi-line string: keep the indentation
    return -1
  elseif empty(stack)
    return 0
  endif
  let [l, col, c] = stack[-1]
  " byte column to screen column
  let vcol = strdisplaywidth(strpart(getline(l), 0, col))
  return c ==# '(' ? vcol + 3 : vcol + 1
endfunction
