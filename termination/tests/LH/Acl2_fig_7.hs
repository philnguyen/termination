module Acl2_fig_7 where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Fig 7
f7 :: Int -> Int
f7 x | x <= 1         = 0
     | x `mod` 2 == 1 = f7 (x+1)
     | otherwise      = 1 + f (x `div` 2)
