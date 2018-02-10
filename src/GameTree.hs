
module GameTree where

import Protolude    hiding (evaluate)

import Color
import Board.Board
import Board.Next
import Data.Tree

tree :: Int -> Color -> Board -> Tree Board
tree 0 _ brd = Node brd []
tree d col brd = Node brd [tree d' col' brd' | brd' <- allPositions brd col]
    where
        d'   = pred d
        col' = succ col


evaluated :: Board -> Int -> Tree Int
evaluated b n = map evaluate $ tree n White b

bestSequence :: Board -> Int -> Board
bestSequence b n = maximum $ tree n White b

