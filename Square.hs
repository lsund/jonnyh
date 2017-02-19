
module Square where

import Data.Char

import Piece

data Square = Square {
      _file    :: Char
    , _rank    :: Int
    , _content :: Maybe Piece
}

instance Show Square where
    show (Square f r Nothing) = f : intToDigit r : "[ ]"
    show (Square f r _)       = f : intToDigit r : "[x]"

instance Eq Square where
    (Square f r _) == (Square f' r' _) = f == f' && r == r'

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest

