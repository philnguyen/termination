module LH_tfact where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

{-@ tfac :: Nat -> n:Nat -> Nat / [n] @-}
tfac :: Int -> Int -> Int
tfac x n | n == 0    = x
         | otherwise = tfac (n*x) (n-1)
