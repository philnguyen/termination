module LH_map where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

map :: (a -> b) -> [a] -> [b]
map f (x : xs) = f x : map f xs
map f []       = []
