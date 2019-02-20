module Isabelle_perm where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Section 5
{-@ p :: Nat -> Nat -> Nat -> Nat @-}
p :: Int -> Int -> Int -> Int
p m n r | 0 < r     = p m (r-1) n
        | 0 < n     = p r (n-1) m
        | otherwise = m
