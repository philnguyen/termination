module Sct_5 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 5
{-@ f :: [a] -> [a] -> [a] @-}
f :: [a] -> [a] -> [a]
f x [] = x
f [] y = f y (tail y)
f x y = f y (tail x)
