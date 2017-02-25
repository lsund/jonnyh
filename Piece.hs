
module Piece where 

data Color  = Black | White deriving (Show, Eq)

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
}

instance Show Piece where
    show (Piece Black Pawn)   = "p"
    show (Piece Black Bishop) = "b"
    show (Piece Black Knight) = "n"
    show (Piece Black Rook)   = "r"
    show (Piece Black Queen)  = "q"
    show (Piece Black King)   = "k"
    show (Piece White Pawn)   = "P"
    show (Piece White Bishop) = "B"
    show (Piece White Knight) = "N"
    show (Piece White Rook)   = "R"
    show (Piece White Queen)  = "Q"
    show (Piece White King)   = "K"


