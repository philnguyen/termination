module Sct_2 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 2: Translated from original program, which the type system could not express
data Tree a = L | N (Tree a) (Tree a)
f2 :: Tree t -> Tree t -> Tree t
f2 L x = x
f2 (N a b) x = g2 a x (N a b)
g2 :: Tree t -> Tree t -> Tree t -> Tree t
g2 a b c = f2 a (N b c)
