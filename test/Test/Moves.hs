
module Test.Moves where

import           Test.HUnit

import           JonnyH.Board
import           JonnyH.Piece.Knight as Knight
import           JonnyH.Square

import           Test.Piece.Pawn
import           Test.Positions


testKnight = TestCase
                (assertEqual
                    "A knight can move to the correct positions"
                    (Knight.moves (Square 'c' 5) p1)
                    m1)

tests = TestList [testKnight, testPawn]

