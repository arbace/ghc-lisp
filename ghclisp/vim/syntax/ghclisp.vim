" Vim syntax file for ghc-lisp (.hsl): EDN structure, Haskell lexemes.
" See ghclisp/SPEC.md.

if exists('b:current_syntax')
  finish
endif

syntax case match

" Vim prefers the match defined last when two start at the same column, so
" the general ones (operators, prefixes, names) come first and the specific
" ones (comments, literals, special heads) after them.
" Names: constructors (and qualified names) and operators.
syntax match ghclispConstructor "\<\%([A-Z][a-zA-Z0-9_']*\.\)*[A-Z][a-zA-Z0-9_']*#*\>"
syntax match ghclispOperator "\%(^\|[ \t([]\)\@<=[-!#$%&*+./<=>?@\\^|~:]\+\%([ \t)\]\n]\)\@="

" Prefixes: promotion and name quotes, type arguments, labels, implicit
" parameters.
syntax match ghclispPrefix "''\=\%([A-Za-z_(\[:]\)\@="
syntax match ghclispPrefix "@\%([A-Za-z_('\["]\)\@="
syntax match ghclispLabel "#[A-Za-z_][A-Za-z0-9_']*"
syntax match ghclispImplicit "?[a-z_][A-Za-z0-9_']*"

" Comments; Haddock comments are ;;| ;;^ ;;* ;;$name (SPEC.md §10).
syntax match ghclispComment ";.*$" contains=ghclispTodo,@Spell
syntax match ghclispDocComment ";;[|^*$].*$" contains=ghclispTodo,@Spell
syntax keyword ghclispTodo contained TODO FIXME XXX NB

" Literals: Haskell's own grammar (D2).
syntax region ghclispString start=+"+ skip=+\\\\\|\\"+ end=+"#\=+ contains=ghclispEscape,@Spell
syntax match ghclispEscape contained +\\\([abfnrtv\\"'&]\|\^[A-Z@\[\]\\^_]\|[0-9]\+\|x[0-9a-fA-F]\+\|o[0-7]\+\|[A-Z][A-Z0-9]*\)+
syntax match ghclispChar "'\%([^\\']\|\\[^']\+\)'#\="
syntax match ghclispNumber "\<\%(0[xX][0-9a-fA-F_]\+\%(\.[0-9a-fA-F_]*\)\=\%([pP][-+]\=[0-9_]\+\)\=\|0[oO][0-7_]\+\|0[bB][01_]\+\|[0-9][0-9_]*\%(\.[0-9_]\+\)\=\%([eE][-+]\=[0-9_]\+\)\=\)#\{,2}\%(\%(Int\|Word\)\%(8\|16\|32\|64\)\=\)\=\>"

" EDN keywords head the forms with no Haskell keyword (D1): :tuple, :infix, ...
syntax match ghclispKeyword ":[a-zA-Z_][-a-zA-Z0-9_']*"

" Haskell keywords and reserved operators as heads (D9).
syntax match ghclispSpecial "(\@<=\%(case\|class\|data\|default\|deriving\|do\|mdo\|foreign\|if\|import\|infixl\|infixr\|infix\|instance\|let\|module\|newtype\|type\|where\|forall\|rec\|proc\|static\|pattern\|then\)\>"
syntax match ghclispSpecial "(\@<=\%(::\|=>\|->\.\=\|<-\|=\||\|\\cases\|\\case\|\\\|-<<\|-<\|>>-\|>-\)\%([ \t\n)]\)\@="
syntax keyword ghclispWord qualified as hiding safe family role stock anyclass via group by using

highlight default link ghclispComment Comment
highlight default link ghclispDocComment SpecialComment
highlight default link ghclispTodo Todo
highlight default link ghclispString String
highlight default link ghclispEscape SpecialChar
highlight default link ghclispChar Character
highlight default link ghclispNumber Number
highlight default link ghclispKeyword Special
highlight default link ghclispSpecial Statement
highlight default link ghclispWord Keyword
highlight default link ghclispConstructor Type
highlight default link ghclispOperator Operator
highlight default link ghclispPrefix Special
highlight default link ghclispLabel Identifier
highlight default link ghclispImplicit Identifier

let b:current_syntax = 'ghclisp'
