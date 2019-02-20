module Ho_sct_fg where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Beginning of 1.2
g :: (a -> a) -> a -> a
g r a = r (r a)
{-@ f :: Nat -> Nat -> Nat @-}
f :: Int -> Int -> Int
f n | n == 0    = \x -> x+1
    | otherwise = g (f (n-1))
