
module JonnyH.Piece.Pawn where

import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Square


moves :: Square -> Color -> Board -> [Square]
moves sqr@(Square f r) White b =
        catMaybes $
            [   neighbor sqr b North
            ,   neighborIfOccupied sqr b NorthWest
            ,   neighborIfOccupied sqr b NorthEast
            ]
            ++ if r == 2 then [Just $ Square f 4 | _rank sqr == 2] else []
moves sqr@(Square f r) Black b =
        catMaybes $
            [   neighbor sqr b South
            ,   neighborIfOccupied sqr b SouthWest
            ,   neighborIfOccupied sqr b SouthEast
            ]
            ++ if r == 7 then [Just $ Square f 5 | _rank sqr == 7] else []


