
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

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest

