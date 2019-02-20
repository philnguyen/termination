module LH_range where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

{-@ range :: lo:Nat -> {hi:Nat|hi>=lo} -> [Nat] / [hi-lo] @-}
range :: Int -> Int -> [Int]
range lo hi | lo < hi   = lo : range (lo+1) hi
            | otherwise = []
