module Sct_3 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 3
{-@ a :: m:Nat -> n:Nat -> Nat / [m,n] @-}
a :: Int -> Int -> Int
a m n | m == 0    = n + 1
      | n == 0    = a (m-1) 1
      | otherwise = a (m-1) (a m (n-1))
