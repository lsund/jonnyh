
module Main where

import Protolude

import JonnyH.Color
import JonnyH.MinMax
import JonnyH.Positions

main :: IO ()
main = do
    let (Evaluation a b) = minmax' 4 (tree' White [b2])
    print a
    print b

