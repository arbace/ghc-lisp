{-# LANGUAGE LambdaCase #-}
module Lisp006 (Shape(..), area, classify) where

import qualified Data.Map.Strict as M

data Shape = Circle Double | Rect { w, h :: Double }
  deriving (Show, Eq)

area :: Shape -> Double
area (Circle r) = pi * r * r
area (Rect w h) = w * h

classify :: (Ord a, Num a) => a -> String
classify n
  | n < 0 = "negative"
  | otherwise = big
  where big = "positive"

table :: M.Map Int String
table = M.fromList [(i, show i) | i <- [1 .. 10], even i]

ops :: [Int -> Int]
ops = [(+ 1), (2 *), subtract 3, \case 0 -> 1; n -> n `div` 2]
