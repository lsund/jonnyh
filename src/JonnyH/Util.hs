
module JonnyH.Util where

import           Data.Maybe
import           Protolude

firstJust :: [Maybe a] -> Maybe a
firstJust xs = fromMaybe Nothing (find isJust xs)
