module LH_gcd where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

{-@ gcd :: a:Nat -> {b:Nat|b<a} -> Nat @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)
