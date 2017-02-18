
module Pawn where

import Chess
import Square

pawnMoves :: Piece -> Square -> [Square]
pawnMoves (Piece White Pawn) sqr@(Square col 2) = 
    valids [inDirection North sqr 1
            , inDirection NorthWest sqr 1 
            , inDirection NorthEast sqr 1]
            ++ [Square col 4 | row sqr == 2]
pawnMoves (Piece Black Pawn) sqr@(Square col 7) = 
    valids [inDirection South sqr 1
            , inDirection SouthWest sqr 1
            , inDirection SouthEast sqr 1]
            ++ [Square col 5 | row sqr == 7]
pawnMoves (Piece _ _) _ = undefined

