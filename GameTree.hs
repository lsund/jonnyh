
module GameTree where

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

-- instance Ord MinMax where
    -- compare a b = 
    -- compare :: MinMax -> MinMax -> Ordering

-- compareMinMax (a : as) (b : bs) n
    -- | even n =
