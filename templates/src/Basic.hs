module Basic where

import Data.Text (Text, pack)


showT :: Show a => a -> Text
showT = pack . show

flipFlop :: Bool -> [Bool]
flipFlop start = start : (concat $ [flipFlop $ not start])
