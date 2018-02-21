
module Test.Moves where

import           Test.HUnit

import           JonnyH.Board
import           JonnyH.Square

import           Test.Positions


testKnight = TestCase (assertEqual "" ms m1)
    where ms = knightMoves (Square 'c' 5) p1


tests = testKnight

