module FOSC where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 1

{-@ r1 :: [a] -> [a] -> [a] @-}
r1 :: [a] -> [a] -> [a]
r1 ls a | null ls   = a
        | otherwise = r1 (tail ls) (head ls : a)

{-@ rev :: [a] -> [a] @-}
rev :: [a] -> [a]
rev ls = r1 ls []


----- Ex 2: Can't type in ML-like type system

----- Ex 3
{-@ a :: Nat -> Nat -> Nat @-}
a :: Int -> Int -> Int
a m n | m == 0    = n + 1
      | n == 0    = a (m-1) 1
      | otherwise = a (m-1) (a m (n-1))
     
 
----- Ex 4
{-@ p :: Nat -> Nat -> Nat -> Nat @-}
p :: Int -> Int -> Int -> Int
p m n r | r > 0     = p m (r-1) n
        | n > 0     = p r (n-1) m
        | otherwise = m
        
----- Ex 5
{-@ f :: [a] -> [a] -> [a] @-}
f :: [a] -> [a] -> [a]
f x y | null y    = x
      | null x    = f y (tail y)
      | otherwise = f y (tail x)
      
----- Ex 6
{-@ f6,g6 :: [a] -> [a] -> [a] @-}
f6, g6 :: [a] -> [a] -> [a]
f6 a b | null b    = g6 a []
       | otherwise = f6 (head b : a) (tail b)
g6 c d | null c    = d
       | otherwise = g6 (tail c) (head c : d)
