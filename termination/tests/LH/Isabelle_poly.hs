module Krauss where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

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
