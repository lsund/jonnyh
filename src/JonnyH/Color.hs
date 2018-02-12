
module JonnyH.Color where

import Protolude

data Color  = Black | White deriving (Show, Eq)

instance Enum Color where
    succ White = Black
    succ Black = White
    toEnum n | n `mod` 2 == 0 = White
    toEnum _ = Black
    fromEnum White = 0
    fromEnum Black = 1

