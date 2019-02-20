module Isabelle_f where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Section 3
{-@ f :: Nat -> Nat -> Nat @-}
f :: Int -> Int -> Int
f n 0 = n
f 0 m | m > 0 = f (m-1) (m-1)
f n m | n > 0 && m > 0 = f (m-1) (n-1)
