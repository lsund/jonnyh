
module JonnyH.Piece.Pawn where

-- import           Control.Monad.Extra
import           Data.Maybe
import           Protolude

import           JonnyH.Board
import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Piece.Common
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

isPawn :: Piece -> Bool
isPawn (Piece _ Pawn) = True
isPawn _              = False

-- source :: Color -> Square -> Board -> Maybe Square
source c dst b =
    let
        dir               = case c of White -> South; Black -> North
        preceedingSquares = [relative 1 dst b dir, relative 2 dst b dir]
        preceedingPieces  = filter (isJust . fst) $ map squareToPiece preceedingSquares
    in
        filterM (fmap correctPiece <$> fst) preceedingPieces
    where
        squareToPiece sqr     = (maybe Nothing (`pieceAt` b) sqr, sqr)
        correctPiece x = isPawn x && (_color x == White)


