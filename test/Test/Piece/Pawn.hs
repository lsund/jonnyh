
module Test.Piece.Pawn where

import           Protolude
import           Test.HUnit

import           JonnyH.Color
import           JonnyH.Piece.Common
import           JonnyH.Initial
import           JonnyH.Piece.Pawn
import           JonnyH.Square

testPawn = TestCase
                (assertEqual
                    "Get the right source pawn based on a destination"
                    (source White (Square 'd' 4) initialBoard)
                    (Just (Square 'd' 2, Piece White Pawn)))
