
module Test.Piece.Pawn where

import           Data.Set            (fromList)
import           Protolude
import           Test.HUnit

import           JonnyH.Color
import           JonnyH.Initial
import           JonnyH.Piece.Common
import           JonnyH.Piece.Pawn   as Pawn
import           JonnyH.Square

import           Test.Positions

testSource = TestCase
                (assertEqual
                    "Get the right source pawn based on a destination"
                    (source White (Square 'd' 4) initialBoard)
                    (Just (Square 'd' 2, Piece White Pawn)))


testMoves = TestList
                [ TestCase
                    (assertEqual
                        "Pawn moves correctly"
                        m3
                        (Pawn.moves (Square 'c' 5) White p3))
                , TestCase
                    (assertEqual
                        "Pawn moves correctly"
                        (fromList m4)
                        (fromList (Pawn.moves (Square 'd' 2) White p4)))
                ]

testPawn = TestList [testSource, testMoves]
