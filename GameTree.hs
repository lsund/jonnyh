
module GameTree where

import Data.List

import Moves
import Board
import Piece

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
tree d col brd = Node brd [tree d' col' brd' | brd' <- allPositions col brd]
    where 
        d'   = pred d
        col' = succ col

evaluatedN :: Int -> Board -> Tree (Board, Int)
evaluatedN n brd = fmap (\brd -> (brd, evaluate brd)) (tree n White brd)

newtype MinMax = MinMax [Int]

-- compares two lists. A list is greater than another list, if the odd elements
-- compare GT and the even elements compare LT
compareValues as bs =
    let 
        comps = zipWith compare as bs
        maxs = head comps : every 2 (tail comps)
        mins = every 2 comps
        gts = filter (== GT) maxs
        lts = filter (== LT) mins

        comps' = zipWith compare bs as
        maxs' = head comps' : every 2 (tail comps')
        mins' = every 2 comps'
        gts' = filter (== GT) maxs'
        lts' = filter (== LT) mins'
    in 
        (length gts + length lts) `compare` (length gts' + length lts')

every n xs = 
    case drop (pred n) xs of
        (y : ys) -> y : every n ys
        []       -> []

