
module JonnyH.Piece.Pawn where

import           Data.Maybe
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Piece.Common
import           JonnyH.Square


moves :: Square -> Color -> Board -> [Square]
moves sqr c b = catMaybes $ neighbours ++ [doubleMove sqr c]
    where
        dirs White = [North, NorthWest, NorthEast]
        dirs Black = [South, SouthWest, SouthEast]
        nbfns      = [neighbor , neighborIfOccupied, neighborIfOccupied]
        neighbours = zipWith ($) (map (($ b) . ($ sqr)) nbfns) (dirs c)


doubleMove :: Square -> Color -> Maybe Square
doubleMove (Square f 2) White = Just $ Square f 4
doubleMove (Square f 7) Black = Just $ Square f 5
doubleMove _ _                = Nothing


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

