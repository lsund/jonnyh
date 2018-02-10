
module MinMax where

import Protolude    hiding (evaluate)

import Color
import Board.Board
import Board.Next
import Data.Tree

minmax :: Int -> Tree Board -> Int
minmax 0 (Node brd _)          = evaluate brd
minmax depth (Node _ children) =
    maximum $ map (negate . minmax (depth - 1)) children

minmax' :: Int -> Tree Board -> (Int, Board)
minmax' 0 (Node brd _)          = (evaluate brd, brd)
minmax' depth (Node _ children) =
    maximumBy (\(a, _) (c, _) -> a `compare` c) $
        map (\t -> let (a, b) = minmax' (depth - 1) t in (negate a, b)) children

minmax'' :: Int -> Tree [Board] -> (Int, [Board])
minmax'' 0 (Node (x : xs) _)          = (evaluate x, x : xs)
minmax'' depth (Node _ children) =
    maximumBy (\(a, _) (c, _) -> a `compare` c) $
        map (\t -> let (a, b) = minmax'' (depth - 1) t in (negate a, b)) children

tree :: Color -> Board -> Tree Board
tree col brd = Node brd [tree (succ col) brd' | brd' <- allPositions brd col]

tree' :: Color -> [Board] -> Tree [Board]
tree' col (b : bs) =
    Node (b : bs) [tree' (succ col) (b' : b: bs) | b' <- allPositions b col]
tree' _ [] = Node [] []

