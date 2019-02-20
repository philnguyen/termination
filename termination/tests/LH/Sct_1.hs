module Sct_1 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 1

{-@ r1 :: [a] -> [a] -> [a] @-}
r1 :: [a] -> [a] -> [a]
r1 [] a = a
r1 ls a = r1 (tail ls) (head ls : a)

{-@ rev :: [a] -> [a] @-}
rev :: [a] -> [a]
rev ls = r1 ls []
