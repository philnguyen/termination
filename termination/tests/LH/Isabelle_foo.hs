module Isabelle_foo where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

{-@ foo :: Bool -> Nat -> Nat -> Nat @-}
foo :: Bool -> Int -> Int -> Int
foo True  n m | n > 0  = foo True (n-1) (m-1)
              | n == 0 = foo False 0 m
foo False n m | m > 0 = foo False (n-1) (m-1)
              | m == 0 = n
