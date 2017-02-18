
module Square where

import Data.Char

import Piece

data Square = Square {
      _file    :: Char
    , _rank    :: Int
    , _content :: Maybe Piece
}

instance Show Square where
    show (Square f r c) = f : [intToDigit r]

data Direction  = North
                | NorthEast
                | East
                | SouthEast
                | South
                | SouthWest
                | West
                | NorthWest

