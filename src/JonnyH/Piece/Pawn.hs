
module JonnyH.Piece.Pawn where

import           Control.Monad.Extra
import           Data.Maybe
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Piece.Common
import           JonnyH.Square
import           JonnyH.Util


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

isPawn :: Piece -> Bool
isPawn (Piece _ Pawn) = True
isPawn _              = False


source :: Color -> Square -> Board -> Maybe (Square, Piece)
source c dst b =
    let
        dir               = backwards c
        preceedingSquares = take 2 $ untilOccupied dst b dir
        maybePieces       = map withPiece preceedingSquares
    in
         find myPawn (catMaybes maybePieces)
    where
        withPiece sqr = (,) sqr <$> pieceAt sqr b
        myPawn (_, p) = isPawn p && (_color p == c)

