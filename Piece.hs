
module Piece where 

data Color  = Black | White deriving (Show)

data Type  = Pawn
           | Bishop
           | Knight
           | Rook
           | Queen
           | King
            deriving (Show)

data Piece = Piece {
      _color :: Color
    , _type :: Type
} deriving (Show)

