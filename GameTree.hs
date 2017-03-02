
module GameTree where

import Moves
import Board
import Piece

data Tree a = Node {
    _content :: Board,
    _children :: [Tree Board]
} deriving (Show)

singleton :: Board -> Tree Board
singleton brd = Node brd []

grow :: Color -> Tree Board -> Tree Board
grow col (Node brd []) = 
    Node brd $ map singleton $ allPositions col brd
grow col (Node brd children) = Node brd $ map (grow $ succ col) children

