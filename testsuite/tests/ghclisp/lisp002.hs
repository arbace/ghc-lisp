-- A Haskell module importing a ghc-lisp module, which imports Haskell.
import Geo.Shape

main :: IO ()
main = mapM_ (\s -> putStrLn (show s ++ ": " ++ describe s)) [Circle 1, Rect 2 8]
