
module JonnyH.MinMax where

import Protolude    hiding (evaluate)
import Data.Tree

import JonnyH.Color
import JonnyH.Board.Board
import JonnyH.Board.Next

minmax :: Int -> Tree Board -> Int
minmax 0 (Node brd _)          = evaluate brd
minmax depth (Node _ children) =
    maximum $ map (negate . minmax (depth - 1)) children


data Evaluation = Evaluation { _value :: Int
                             , _sequence :: [Board]
                             }


instance Eq Evaluation where
    a == b = _value a == _value b


instance Ord Evaluation where
    a `compare` b = _value a `compare` _value b


mapValue :: (Int -> Int) -> Evaluation -> Evaluation
mapValue f (Evaluation v xs) = Evaluation (f v) xs


minmax' :: Int -> Tree [Board] -> Evaluation
minmax' 0 (Node (x : xs) _)          = Evaluation (evaluate x) (x : xs)
minmax' depth (Node _ children) =
    maximum $ map (mapValue negate . minmax' (depth - 1)) children

tree :: Color -> Board -> Tree Board
tree col brd = Node brd [tree (succ col) brd' | brd' <- allPositions brd col]

tree' :: Color -> [Board] -> Tree [Board]
tree' col (b : bs) =
    Node (b : bs) [tree' (succ col) (b' : b : bs) | b' <- allPositions b col]
tree' _ [] = Node [] []

