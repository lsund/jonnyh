
module Test.Moves where

import           Data.Set            (fromList)
import           Protolude
import           Test.HUnit

import           JonnyH.Board
import           JonnyH.Piece.Bishop as Bishop
import           JonnyH.Piece.Knight as Knight
import           JonnyH.Square

import           Test.Piece.Pawn
import           Test.Positions


testKnight = TestCase
                (assertEqual
                    "A knight can move to the correct positions"
                    (Knight.moves (Square 'c' 5) p1)
                    m1)

testBishop = TestCase
                (assertEqual
                    "A bishop can move to the correct positions"
                    (fromList (Bishop.moves (Square 'c' 5) p2))
                    (fromList m2))

tests = TestList [testBishop, testKnight, testPawn]

