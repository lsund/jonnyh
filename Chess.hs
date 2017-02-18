
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

rows :: [Int]
rows = [1..8]

columns :: String
columns = ['a'..'h']

