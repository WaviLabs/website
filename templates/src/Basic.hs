module Basic where

import Data.Text (Text, pack)


showT :: Show a => a -> Text
showT = pack . show

flipFlop :: Bool -> [Bool]
flipFlop start = start : (concat $ [flipFlop $ not start])

tripZip :: [a] -> [b] -> [c] -> [(a, b, c)]
tripZip (x:xs) (y:ys) (z:zs) = (x,y,z) : tripZip xs ys zs
tripZip [] [] [] = []
tripZip _ _ _    = []

uncurrry :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurrry f = \(x, y, z) -> f x y z
