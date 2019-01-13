
module JonnyH.Util where

import           Data.Maybe
import           Protolude

firstJust :: [Maybe a] -> Maybe a
firstJust xs = fromMaybe Nothing (find isJust xs)

takeWhilePlus1 :: (a -> Bool) -> [a] -> [a]
takeWhilePlus1 p = foldr (\x ys -> if p x then x : ys else [x]) []

normalize :: [Int] -> [Double]
normalize xs = map (\x -> fromIntegral x / fromIntegral (sum xs)) xs

-- normalizedWithSpans :: [Int] -> [Double]

applyToSnds :: ([b] -> [c]) -> [(a, b)] -> [(a, c)]
applyToSnds f xs = zip (map fst xs) (f (map snd xs))
