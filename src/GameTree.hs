
module GameTree where

import Protolude    hiding (evaluate)

import Color
import Board.Board
import Board.Next
import Data.Tree

minmax :: Int -> Tree Board -> Int
minmax 0 (Node brd _)          = evaluate brd
minmax depth (Node _ children) =
    maximum $ map (negate . minmax (depth - 1)) children

tree :: Color -> Board -> Tree Board
tree col brd = Node brd [tree (succ col) brd' | brd' <- allPositions brd col]
    where
        col' = succ col

