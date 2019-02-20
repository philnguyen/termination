module Acl2_fig_2 where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Fig 2
{-@ f :: n:Int -> Int / [n * n] @-}
f :: Int -> Int
f x | x == 0    = 0
    | x < 0     = f (x+1)
    | otherwise = f (x-1)
dec :: Int -> Int
dec x | x <= 0    = 255
      | otherwise = x-1
foo :: Int -> Int -> Int
foo i j | i == 1    = if j == 1 then 0 else foo (dec j) (dec j)
        | otherwise = foo (dec i) j
