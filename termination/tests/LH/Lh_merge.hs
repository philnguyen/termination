module LH_merge where
import Prelude hiding ((!!), map, gcd)
import Language.Haskell.Liquid.Prelude

{-@ merge :: xs:_ -> ys:_ -> _ / [len xs + len ys] @-}
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys
