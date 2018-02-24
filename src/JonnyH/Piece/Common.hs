
module JonnyH.Piece.Common where

import           GHC.Show
import           Protolude    hiding (Type)

import           JonnyH.Color

data Type  = Pawn
           | Bishop
           | Knight
           | Rook
           | Queen
           | King
            deriving (Show, Eq)

data Piece = Piece {
      _color :: Color
    , _type  :: Type
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
value (Piece _ King)   = 99

isWhite :: Piece -> Bool
isWhite (Piece Black _ ) = False
isWhite (Piece White _ ) = True
