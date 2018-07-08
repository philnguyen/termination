module Krauss where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Section 3
{-@ f :: Nat -> Nat -> Nat @-}
f :: Int -> Int -> Int
f n 0 = n
f 0 m | m > 0 = f (m-1) (m-1)
f n m | n > 0 && m > 0 = f (m-1) (n-1)


----- Section 5
{-@ p :: Nat -> Nat -> Nat -> Nat @-}
p :: Int -> Int -> Int -> Int
p m n r | 0 < r     = p m (r-1) n
        | 0 < n     = p r (n-1) m
        | otherwise = m

{-@ foo :: Bool -> Nat -> Nat -> Nat @-}
foo :: Bool -> Int -> Int -> Int
foo True  n m | n > 0  = foo True (n-1) (m-1)
              | n == 0 = foo False 0 m
foo False n m | m > 0 = foo False (n-1) (m-1)
              | m == 0 = n

{-@ bar :: Nat -> Nat -> Nat -> Nat @-}
bar :: Int -> Int -> Int -> Int
bar 0 n m | n > 0 = bar m m m
bar v n m | v > 0 = 0
bar k 0 m = 0

data Poly = Pc Int | Pinj Int Poly | PX Poly Int Poly
add :: Poly -> Poly -> Poly
add (Pc a) (Pc b) = Pc (a+b)
add (Pc c) (Pinj i p) = Pinj i (add p (Pc c))
add (Pc c) (PX p i q) = PX p i (add q (Pc c))
add (Pinj x p) (Pinj y q) | x == y    = Pinj x (add p q)
                          | y < x     = Pinj y (add (Pinj (x-y) p) q)
                          | otherwise = add (Pinj y q) (Pinj x p)
add (Pinj x p) (PX q y r) | x == 0    = add p (PX q y r)
                          | x == 1    = PX q y (add p r)
                          | otherwise = PX q y (add (Pinj (x-1) p) r)
add (PX p1 x p2) (PX q1 y q2) | x == y    = PX (add p1 q1) x (add p2 q2)
                              | y < x     = PX (add (PX p1 (x-y) (Pc 0)) q1) y (add p2 q2)
                              | otherwise = add (PX q1 y q2) (PX p1 x p2)
add (Pinj i p) (Pc c) = add (Pc c) (Pinj i p)
add (PX p i q) (Pc c) = add (Pc c) (PX p i q)
add (PX q y r) (Pinj x p) = add (Pinj x p) (PX q y r)
