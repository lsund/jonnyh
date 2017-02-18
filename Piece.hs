
module Piece where

import Square

data Color  = Black | White deriving (Show)

data Type  = Pawn
           | Bishop
           | Knight
           | Rook
           | Queen
           | King
            deriving (Show)

data Piece = Piece Color Type deriving (Show)

pawnMoves :: Piece -> Square -> [Square]
pawnMoves (Piece White Pawn) sqr@(Square col 2) = 
    valids  [   inDirection North sqr 1
            ,   inDirection NorthWest sqr 1 
            ,   inDirection NorthEast sqr 1
            ]
            ++ [Square col 4 | row sqr == 2]
pawnMoves (Piece Black Pawn) sqr@(Square col 7) = 
    valids  [   inDirection South sqr 1
            ,   inDirection SouthWest sqr 1
            ,   inDirection SouthEast sqr 1
            ]
            ++ [Square col 5 | row sqr == 7]
pawnMoves (Piece _ _) _ = undefined

bishopMoves :: Square -> [Square]
bishopMoves sqr =
        sequenceInDirection sqr NorthEast
    ++  sequenceInDirection sqr SouthEast
    ++  sequenceInDirection sqr SouthWest
    ++  sequenceInDirection sqr NorthWest

knightMoves :: Square -> [Square]
knightMoves sqr =
    valids  [   inDirection North   (inDirection East sqr 1) 2
            ,   inDirection North   (inDirection West sqr 1) 2
            ,   inDirection East    (inDirection North sqr 1) 2
            ,   inDirection East    (inDirection South sqr 1) 2
            ,   inDirection South   (inDirection East sqr 1) 2
            ,   inDirection South   (inDirection West sqr 1) 2
            ,   inDirection West    (inDirection North sqr 1) 2
            ,   inDirection West    (inDirection South sqr 1) 2
            ]

rookMoves :: Square -> [Square]
rookMoves sqr =
        sequenceInDirection sqr North
    ++  sequenceInDirection sqr East
    ++  sequenceInDirection sqr South
    ++  sequenceInDirection sqr West

queenMoves :: Square -> [Square]
queenMoves sqr = bishopMoves sqr ++ rookMoves sqr

kingMoves :: Square -> [Square]
kingMoves sqr =
    valids  [   inDirection North sqr 1
            ,   inDirection NorthEast sqr 1
            ,   inDirection East sqr 1
            ,   inDirection SouthEast sqr 1
            ,   inDirection South sqr 1
            ,   inDirection SouthWest sqr 1
            ,   inDirection West sqr 1
            ,   inDirection NorthWest sqr 1
            ]

moves :: Piece -> Square -> [Square]
moves piece@(Piece _ Pawn) sqr   = pawnMoves piece sqr
moves piece@(Piece _ Bishop) sqr = bishopMoves sqr
moves piece@(Piece _ Rook) sqr   = rookMoves sqr
moves piece@(Piece _ Queen) sqr   = queenMoves sqr

