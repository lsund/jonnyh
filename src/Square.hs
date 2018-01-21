
module Square where

import Data.Char

import Piece

data Square = Square {
      _file    :: Char
    , _rank    :: Int
} deriving (Eq, Ord)

instance Show Square where
    show (Square f r) = f : [intToDigit r]

