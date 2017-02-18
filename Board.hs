
module Board where

import Data.List.Split

import Square

newtype Board = Board [Square] deriving (Show)

