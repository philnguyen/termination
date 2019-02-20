module Acl2_fig_6 where
import Prelude hiding ((!!), length, foldl, foldr, reverse, append, concat)
import Language.Haskell.Liquid.Prelude

----- Fig 6
f6, g, h :: Int -> Int
f6 x | x == 0 = 0
     | x < 0 = g x
     | otherwise = h x
g x = f6 (x+1)
h x = f6 (x-1)
