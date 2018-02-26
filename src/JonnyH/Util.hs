
module JonnyH.Util where

import           Data.Maybe
import           Protolude

firstJust :: [Maybe a] -> Maybe a
firstJust xs = fromMaybe Nothing (find isJust xs)

accumJust :: Maybe a -> [a] -> [a]
accumJust (Just x) acc = x : acc
accumJust Nothing acc = acc
