module HOSC where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Beginning of 1.2
g :: (a -> a) -> a -> a
g r a = r (r a)
{-@ f :: Nat -> Nat -> Nat @-}
f :: Int -> Int -> Int
f n | n == 0    = \x -> x+1
    | otherwise = g (f (n-1))
    

----- Later in 1.2
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr h a [] = a
foldr h a xs = h (head xs) (foldr h a (tail xs))
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl h a [] = a
foldl h a xs = foldl h (h a (head xs)) (tail xs) 
reverse :: [a] -> [a]
reverse xs = foldl (\ys x -> x:ys) [] xs
append :: [a] -> [a] -> [a]
append xs ys = foldr (:) xs ys
concat :: [[a]] -> [a]
concat xss = foldr append [] xss


----- Ex in 5.3: can't type Y!
