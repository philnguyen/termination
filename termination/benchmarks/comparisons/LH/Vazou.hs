module Vazou where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

{-@ gcd :: a:Nat -> {b:Nat|b<a} -> Nat @-}
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

{-@ tfac :: Nat -> n:Nat -> Nat / [n] @-}
tfac :: Int -> Int -> Int
tfac x n | n == 0    = x
         | otherwise = tfac (n*x) (n-1)

{-@ range :: lo:Nat -> {hi:Nat|hi>=lo} -> [Nat] / [hi-lo] @-}
range :: Int -> Int -> [Int]
range lo hi | lo < hi   = lo : range (lo+1) hi
            | otherwise = []
            
map :: (a -> b) -> [a] -> [b]
map f (x : xs) = f x : map f xs
map f []       = []

{-@ merge :: xs:_ -> ys:_ -> _ / [len xs + len ys] @-}
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys
