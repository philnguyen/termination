module Sct_6 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 6
{-@ f6,g6 :: [a] -> [a] -> [a] @-}
f6, g6 :: [a] -> [a] -> [a]
f6 a [] = g6 a []
f6 a b = f6 (head b : a) (tail b)
g6 [] d = d
g6 c d = g6 (tail c) (head c : d)
