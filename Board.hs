
module Board where

import Data.List
import Data.Maybe
import Data.Char

import Direction
import Square
import Piece

newtype Board = Board { _squares :: [Square] } 

instance Show Board where
    show board = 
        "   A  B  C  D  E  F  G  H\n" ++
        concat numln
        where
            rs = ranks board
            rsln = reverse $  map ((++ "\n") . concatMap show) rs
            numln = zipWith (\x ln -> show x ++ " " ++ ln) [8,7..1] rsln
    
ranks board = ranks' $ _squares board  
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

empty :: Square -> Bool
empty (Square _ _ Nothing) = True
empty _                    = False

findSquare :: Square -> Board -> Maybe Square
findSquare sqr (Board sqrs) = find (== sqr) sqrs

valids :: [Maybe Square] -> [Square]
valids msqrs = map fromJust $ filter isJust msqrs

at :: Char -> Int -> Board -> Maybe Square
at f r = findSquare (Square f r Nothing)

relative :: Int -> Square ->  Board -> Direction -> Maybe Square
relative n (Square f r c) board dir = 
    case dir of 
        North     -> findSquare (Square f       nNorth  c) board
        NorthEast -> findSquare (Square nEast   nNorth  c) board
        East      -> findSquare (Square nEast   r       c) board
        SouthEast -> findSquare (Square nEast   nSouth  c) board
        South     -> findSquare (Square f       nSouth  c) board
        SouthWest -> findSquare (Square nWest   nSouth  c) board
        West      -> findSquare (Square nWest   r       c) board
        NorthWest -> findSquare (Square nWest   nNorth  c) board
    where 
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n

neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1

emptyInDirection :: Square -> Direction -> Board -> [Square]
emptyInDirection sqr dir board = 
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhile empty (reverse reverseSeq)
    where
        nth xs n = 
            case relative n sqr board dir of
                Just sqr -> sqr : xs
                Nothing -> [] ++ xs

inDirection :: Square ->Board -> Direction -> [Square]
inDirection sqr board dir = 
    let sqrs = emptyInDirection sqr dir board
    in 
        if not (null sqrs) then
            next (head sqrs) dir board ++ sqrs
        else 
            []
    where
        next sqr dir board = maybeToList $ neighbor sqr board dir

indexOf :: Square -> Board -> Int
indexOf (Square f r _) board = (pred fNum * 8) + pred r
    where fNum = ord f - 96

swapContent :: Square -> Square -> Board -> Board
swapContent sqr@(Square f r pce) sqr'@(Square f' r' pce') board = 
    let 
        ind  = indexOf sqr board
        before = take ind (_squares board)
        after  = drop (succ ind) (_squares board)
        board'  = Board $ before ++ [Square f r pce'] ++ after
    in
        let 
            ind' = indexOf sqr' board'
            before' = take ind' (_squares board')
            after' = drop (succ ind') (_squares board') 
        in
            Board $ before' ++ [Square f' r' pce] ++ after'

