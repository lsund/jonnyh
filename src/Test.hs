
module Test where


import GHC.Show
import Protolude                hiding (show)
import qualified Data.Map as Map


import Color
import Moves
import Board.Board
import Board.Square
import Piece

data Play = Play Board [Square]

instance Show Play where
    show (Play board moves) =
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) [8,7..1] rsRep')
        where
            rs = reverse $ ranks board
            rsRep = concatMap (strRep board)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep board sqr =
                if sqr `elem` moves then "[x]" else
                    case occupiedBy sqr board of
                        Just pce -> "[" ++ show pce ++ "]"
                        Nothing  -> "[ ]"

-------------------------------------------------------------------------------
-- tests

testQueen = board $ Map.fromList
    [
      (Square 'd' 8, Piece White Pawn)
    , (Square 'd' 4, Piece White Queen)
    , (Square 'b' 4, Piece Black Queen)
    ]

testKnight = board $ Map.fromList
    [
      (Square 'e' 5, Piece White Pawn)
    , (Square 'g' 4, Piece White Knight)
    , (Square 'e' 3, Piece Black Queen)
    ]

testPawn = board $ Map.fromList
    [
      (Square 'g' 4, Piece Black Pawn)
    , (Square 'f' 3, Piece Black Queen)
    , (Square 'g' 3, Piece White Queen)
    ]

testKing = board $ Map.fromList
    [
      (Square 'd' 4, Piece Black King)
    , (Square 'd' 3, Piece Black Pawn)
    , (Square 'd' 5, Piece White Pawn)
    ]
