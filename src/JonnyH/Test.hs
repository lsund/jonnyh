
module JonnyH.Test where


import GHC.Show
import Protolude                hiding (show)
import qualified Data.Map as Map

import JonnyH.Color
import JonnyH.Board.Board
import JonnyH.Board.Square
import JonnyH.Piece

data Play = Play Board [Square]

instance Show Play where
    show (Play b moves) =
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) ([8,7..1] :: [Int]) rsRep')
        where
            rs = reverse $ ranks b
            rsRep = concatMap (strRep b)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep b' sqr =
                if sqr `elem` moves then "[x]" else
                    case occupiedBy sqr b' of
                        Just pce -> "[" ++ show pce ++ "]"
                        Nothing  -> "[ ]"

-------------------------------------------------------------------------------
-- tests

testQueen :: Board
testQueen = board $ Map.fromList
    [
      (Square 'd' 8, Piece White Pawn)
    , (Square 'd' 4, Piece White Queen)
    , (Square 'b' 4, Piece Black Queen)
    ]

testKnight :: Board
testKnight = board $ Map.fromList
    [
      (Square 'e' 5, Piece White Pawn)
    , (Square 'g' 4, Piece White Knight)
    , (Square 'e' 3, Piece Black Queen)
    ]

testPawn :: Board
testPawn = board $ Map.fromList
    [
      (Square 'g' 4, Piece Black Pawn)
    , (Square 'f' 3, Piece Black Queen)
    , (Square 'g' 3, Piece White Queen)
    ]

testKing :: Board
testKing = board $ Map.fromList
    [
      (Square 'd' 4, Piece Black King)
    , (Square 'd' 3, Piece Black Pawn)
    , (Square 'd' 5, Piece White Pawn)
    ]
