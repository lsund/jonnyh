
module Chess where

data Type  = Pawn
           | Bishop
           | Knight
           | Rook
           | Queen
           | King
            deriving (Show)

data Color  = Black | White deriving (Show)

data Piece = Piece Color Type deriving (Show)

rows :: String
rows = ['a'..'h']

columns :: [Int]
columns = [1..8]

