
module GameTree where

import Moves
import Board
import Piece

data Tree a = Node {
    _content :: a,
    _children :: [Tree a]
} deriving (Show)

instance Functor Tree where
    fmap f (Node x []) = Node (f x) []
    fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

foldTree :: (a -> b -> b) -> b -> Tree a -> [b]
foldTree f z (Node x []) = [f x z]
foldTree f z (Node x ns) = [f x y | n <- ns, y <- foldTree f z n]

singleton :: Board -> Tree Board
singleton brd = Node brd []

grow :: Color -> Tree Board -> Tree Board
grow col (Node brd []) = 
    Node brd $ map singleton $ allPositions col brd
grow col (Node brd children) = Node brd $ map (grow $ succ col) children

boardN :: Int -> Board -> Tree Board
boardN n brd = iterate (grow White) (singleton brd) !! n

valueN :: Int -> Board -> Tree (Int, Int)
valueN n brd = fmap evaluate (boardN n brd)


