module Ho_sct_fold where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

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
