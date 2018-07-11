module FOSC where
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


----- Ex 2: Translated from original program, which the type system could not express
data Tree a = L | N (Tree a) (Tree a)
f2 :: Tree t -> Tree t -> Tree t
f2 L x = x
f2 (N a b) x = g2 a x (N a b)
g2 :: Tree t -> Tree t -> Tree t -> Tree t
g2 a b c = f2 a (N b c)

----- Ex 3
{-@ a :: m:Nat -> n:Nat -> Nat / [m,n] @-}
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
f x [] = x
f [] y = f y (tail y)
f x y = f y (tail x)
      
----- Ex 6
{-@ f6,g6 :: [a] -> [a] -> [a] @-}
f6, g6 :: [a] -> [a] -> [a]
f6 a [] = g6 a []
f6 a b = f6 (head b : a) (tail b)
g6 [] d = d
g6 c d = g6 (tail c) (head c : d)
