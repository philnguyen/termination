module Manolios where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Fig 2
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
        

----- Fig 6
f6, g, h :: Int -> Int
f6 x | x == 0 = 0
     | x < 0 = g x
     | otherwise = h x
g x = f6 (x+1)
h x = f6 (x-1)


----- Fig 7
f7 :: Int -> Int
f7 x | x <= 1         = 0
     | x `mod` 2 == 1 = f7 (x+1)
     | otherwise      = 1 + f (x `div` 2)
