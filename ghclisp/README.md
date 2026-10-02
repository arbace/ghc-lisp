# ghc-lisp

ghc-lisp is GHC with a second front end: it compiles Haskell written as
s-expressions, in `.hsl` files, next to ordinary `.hs` files. A `.hsl`
module is parsed into the same syntax tree as the Haskell it stands for,
so everything after the parser (renamer, type checker, optimiser, code
generator, GHCi) treats both alike. The design is in [DESIGN.md](DESIGN.md)
and the grammar, form by form, in [SPEC.md](SPEC.md).

```clojure
(module Main [main])

(import qualified Data.Map.Strict as M)

(:: wordCounts (-> String (M.Map String Int)))
(= (wordCounts text)
   (M.fromListWith + [(:tuple w 1) | (<- w (words text))]))

(:: main (IO ()))
(= main
   (do (<- text getContents)
       (mapM_ print (M.toList (wordCounts text)))))
```

## Using it

Build this tree (see `CLAUDE.md`), then use `_build/stage1/bin/ghc` as
you would use `ghc`:

```sh
ghc Main.hsl                 # compile a program
ghc --make Main.hs           # .hs and .hsl modules import each other
runghc Main.hsl
ghci Foo.hsl                 # or :load Foo.hsl
```

The finder looks for `M.hsl` wherever it looks for `M.hs`. Errors and
warnings point into the `.hsl` file. Language extensions and options go at
the top of the file, as forms: `(:language LambdaCase GADTs)`,
`(:options-ghc "-Wall")`. CPP is not supported in `.hsl` files.

Converting:

```sh
ghc --hs2lisp Foo.hs > Foo.hsl     # keeps comments; -- | becomes ;;|
ghc --lisp2hs Foo.hsl > Foo.hs     # with GHC's pretty-printer
ghc --lisp-check Foo.hs ...        # Haskell -> Lisp -> Haskell gives the same tree?
```

Editors: Vim and Neovim support is in [vim/](vim/README.md). Cabal needs
a one-line patch to find `.hsl` modules: see [cabal/](cabal/README.md).

## Cheat sheet

Names are Haskell's own (`sortBy`, `foldl'`, `M.insert`, `:|`). Haskell
keywords and reserved operators head their forms; forms with no keyword
use EDN keywords such as `:tuple`. Vectors are Haskell's brackets.

| Haskell | ghc-lisp |
|---|---|
| `module M (T(..), f) where` | `(module M [(T ..) f])` |
| `import qualified Data.Map as M` | `(import qualified Data.Map as M)` |
| `import Data.List (sortBy)` | `(import Data.List [sortBy])` |
| `f :: Int -> Int` | `(:: f (-> Int Int))` |
| `f x = x + 1` | `(= (f x) (+ x 1))` |
| `f 0 = 1; f n = n * f (n-1)` | `(= (f 0) 1)` `(= (f n) (* n (f (- n 1))))` |
| guards and `where` | `(= (sign n) (\| (< n 0) "neg") (\| otherwise "pos") (where ...))` |
| `f :: Show a => a -> String` | `(:: f (=> (Show a) (-> a String)))` |
| `forall a. a -> a` | `(forall a (-> a a))` |
| `a + b * c` (a chain, fixity as usual) | `(:infix a + b * c)` |
| `a ++ b ++ c` | `(++ a b c)` |
| `(a + b) * c` | `(* (+ a b) c)` |
| `negate`: `-x` | `(- x)` |
| `(+ 1)`, `(2 *)` | `(:section-r + 1)`, `(:section-l 2 *)` |
| `(+) 1` | `(+ 1)` |
| ``a `div` b`` | `(:infix a div b)` or `(div a b)` |
| `\x y -> e` | `(\ x y e)` |
| `\case Just x -> x; Nothing -> 0` | `(\case (-> (Just x) x) (-> Nothing 0))` |
| `case e of p -> a` | `(case e (-> p a))` |
| `if c then a else b` | `(if c a b)` |
| `let x = 1 in e` | `(let (= x 1) e)` |
| `do { x <- m; let y = 2; f x y }` | `(do (<- x m) (let (= y 2)) (f x y))` |
| `(a, b)`, `(a,)` | `(:tuple a b)`, `(:tuple a :_)` |
| `[1, 2, 3]`, `[1 .. 10]` | `[1 2 3]`, `[1 .. 10]` |
| `[x * y \| x <- xs, even x]` | `[(* x y) \| (<- x xs) (even x)]` |
| `e :: Double` | `(:: e Double)` |
| `f @Int` | `(f @Int)` |
| `x@(Just y)`, `~p`, `!p` | `(:as x (Just y))`, `(~ p)`, `(! p)` |
| `data T = A Int \| B { x :: Int }` | `(data T (A Int) (B (:: x Int)))` |
| `deriving (Show, Eq)` | `(deriving [Show Eq])` |
| `newtype N = N { unN :: Int }` | `(newtype N (N (:: unN Int)))` |
| `class Eq a => C a where m :: a` | `(class (=> (Eq a) (C a)) (:: m a))` |
| `instance C Int where m = 0` | `(instance (C Int) (= m 0))` |
| `type Name = String` | `(type Name String)` |
| `T { x = 1 }`, `r { x = 2 }` | `(:rec T (= x 1))`, `(:update r (= x 2))` |
| `r.field` (`OverloadedRecordDot`) | `(:get r field)` |
| `{-# INLINE f #-}` | `(:inline f)` |
| `-- \| doc`, `-- ^ doc` | `;;\| doc`, `;;^ doc` |

Everything else in GHC's syntax (GADTs, type families, Template Haskell,
arrows, linear types, ...) has a form too: see [SPEC.md](SPEC.md).

## How well it works

`ghc --lisp-check` reproduces the parsed tree exactly for every Haskell
file in `compiler/`, `libraries/`, `utils/`, `ghc/` and `hadrian/` that
GHC parses on its own (4,863 files), and for 99.6% of the parseable files
in `testsuite/tests`. 1,069 of the testsuite's runnable programs, converted
to `.hsl`, behave exactly like the originals (`ghclisp/run-corpus.sh`).
