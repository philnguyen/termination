module Isabelle_bar where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

{-@ bar :: Nat -> Nat -> Nat -> Nat @-}
bar :: Int -> Int -> Int -> Int
bar 0 n m | n > 0 = bar m m m
bar v n m | v > 0 = 0
bar k 0 m = 0
