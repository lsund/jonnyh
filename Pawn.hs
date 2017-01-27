
module Pawn where

import Chess
import Square

pawnMoves :: Piece -> Square -> [Square]
pawnMoves (Piece White Pawn) sqr@(col, 2) = 
    valids [neighbor North sqr
            , neighbor NorthWest sqr 
            , neighbor NorthEast sqr]
            ++ [(col, 4) | row sqr == 2]
pawnMoves (Piece Black Pawn) sqr@(col, 7) = 
    valids [neighbor South sqr
            , neighbor SouthWest sqr
            , neighbor SouthEast sqr]
            ++ [(col, 5) | row sqr == 7]
pawnMoves (Piece _ _) _ = undefined

valids = filter (\(x, y) -> elem x rows && elem y columns)

