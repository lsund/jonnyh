
module Piece where

data Color  = Black | White deriving (Show, Eq)

instance Enum Color where
    succ White = Black
    succ Black = White
    toEnum n | n `mod` 2 == 0 = White
    toEnum _ = Black
    fromEnum White = 0
    fromEnum Black = 1

data Type  = Pawn
           | Bishop
           | Knight
           | Rook
           | Queen
           | King
            deriving (Show, Eq)

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


value :: Piece -> Int
value (Piece _ Pawn)   = 1
value (Piece _ Bishop) = 3
value (Piece _ Knight) = 3
value (Piece _ Rook)   = 5
value (Piece _ Queen)  = 9
value _                = undefined

isWhite :: Piece -> Bool
isWhite (Piece Black _ ) = False
isWhite (Piece White _ ) = True
