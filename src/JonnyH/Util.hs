
module JonnyH.Util where

import           Data.Maybe
import           Protolude

firstJust :: [Maybe a] -> Maybe a
firstJust xs = fromMaybe Nothing (find isJust xs)


accumJust :: Maybe a -> [a] -> [a]
accumJust (Just x) acc = x : acc
accumJust Nothing acc = acc


takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 p = foldr (\x ys -> if p x then x : ys else [x]) []


