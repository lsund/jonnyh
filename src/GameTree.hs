
module GameTree where

import Data.List

import Moves
import Board
import Piece
import NextBoard

data Tree a = Node {
    _content  :: a,
    _children :: [Tree a]
} deriving (Show)

instance Functor Tree where
    fmap f (Node x []) = Node (f x) []
    fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

foldTree :: (a -> b -> b) -> b -> Tree a -> [b]
foldTree f z (Node x []) = [f x z]
foldTree f z (Node x ns) = [f x y | n <- ns, y <- foldTree f z n]

tree :: Int -> Color -> Board -> Tree Board
tree 0 col brd = Node brd []
tree d col brd = Node brd [tree d' col' brd' | brd' <- allPositions brd col]
    where
        d'   = pred d
        col' = succ col

intToOrd :: Int -> Ordering
intToOrd n
    | n < 0     = LT
    | n > 0     = GT
    | otherwise = EQ

compareValues :: [(Board, Int)] -> [(Board, Int)] -> Ordering
compareValues as bs = intToOrd $ compareValues' as bs 0
    where
        compareValues' [] [] n = 0
        compareValues' ((_, a) : as) ((_, b) : bs) n
            | even n =
                case compare a b of
                    GT -> succ $ compareValues' as bs (succ n)
                    LT -> pred $ compareValues' as bs (succ n)
                    EQ -> compareValues' as bs $ succ n
            | odd n =
                case compare a b of
                    GT -> pred $ compareValues' as bs (succ n)
                    LT -> succ $ compareValues' as bs (succ n)
                    EQ -> compareValues' as bs (succ n)

bestmove brd n = maximumBy compareValues $
    foldTree (\x acc -> (x, evaluate x) : acc) [] $ tree n White brd

