
module JonnyH.MinMax where

import           Data.Tree
import           Protolude          hiding (evaluate)

import           JonnyH.Board.Board
import           JonnyH.Board.Update
import           JonnyH.Color

minmax :: Int -> Tree Board -> Int
minmax 0 (Node b _)          = evaluate b
minmax depth (Node _ children) =
    maximum $ map (negate . minmax (depth - 1)) children


data Evaluation = Evaluation { _value    :: Int
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
tree col b = Node b [tree (succ col) b' | b' <- allUpdates b col]

tree' :: Color -> [Board] -> Tree [Board]
tree' col (b : bs) =
    Node (b : bs) [tree' (succ col) (b' : b : bs) | b' <- allUpdates b col]
tree' _ [] = Node [] []

