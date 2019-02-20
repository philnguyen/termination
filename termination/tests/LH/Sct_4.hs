module Sct_4 where
import Prelude hiding ((!!), length)
import Language.Haskell.Liquid.Prelude

----- Ex 4
{-@ p :: Nat -> Nat -> Nat -> Nat @-}
p :: Int -> Int -> Int -> Int
p m n r | r > 0     = p m (r-1) n
        | n > 0     = p r (n-1) m
        | otherwise = m
