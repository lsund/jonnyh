
module Board where

import Data.List.Split
import Data.List
import Data.Maybe

import Direction
import Square
import Piece

newtype Board = Board { _squares :: [Square] }

ranks board = ranks' $ _squares board  
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

instance Show Board where
    show board = 
        "   A  B  C  D  E  F  G  H\n" ++
        concat numln
        where
            rs = ranks initialBoard
            rsln = reverse $  map ((++ "\n") . concatMap show) rs
            numln = zipWith (\x ln -> show x ++ " " ++ ln) [8,7..1] rsln
    
initialBoard :: Board
initialBoard = Board $ map initialSquare [(x, y) | x <- ['a'..'h'], y <- [1..8]]

initialSquare :: (Char, Int) -> Square
initialSquare ('a', 1) = Square 'a' 1 (Just (Piece White Rook))
initialSquare ('b', 1) = Square 'b' 1 (Just (Piece White Knight))
initialSquare ('c', 1) = Square 'c' 1 (Just (Piece White Bishop))
initialSquare ('d', 1) = Square 'd' 1 (Just (Piece White Queen))
initialSquare ('e', 1) = Square 'e' 1 (Just (Piece White King))
initialSquare ('f', 1) = Square 'f' 1 (Just (Piece White Bishop))
initialSquare ('g', 1) = Square 'g' 1 (Just (Piece White Knight))
initialSquare ('h', 1) = Square 'h' 1 (Just (Piece White Rook))
initialSquare ( f , 2) = Square f   2 (Just (Piece White Pawn))
initialSquare ( f , 7) = Square f   7 (Just (Piece Black Pawn))
initialSquare ('a', 8) = Square 'a' 8 (Just (Piece Black Rook))
initialSquare ('b', 8) = Square 'b' 8 (Just (Piece Black Knight))
initialSquare ('c', 8) = Square 'c' 8 (Just (Piece Black Bishop))
initialSquare ('d', 8) = Square 'd' 8 (Just (Piece Black Queen))
initialSquare ('e', 8) = Square 'e' 8 (Just (Piece Black King))
initialSquare ('f', 8) = Square 'f' 8 (Just (Piece Black Bishop))
initialSquare ('g', 8) = Square 'g' 8 (Just (Piece Black Knight))
initialSquare ('h', 8) = Square 'h' 8 (Just (Piece Black Rook))
initialSquare (f, r)   = Square f   r Nothing

empty :: Square -> Bool
empty (Square _ _ Nothing) = True
empty _                    = False

findSquare :: Square -> Board -> Maybe Square
findSquare sqr (Board sqrs) = find (== sqr) sqrs

valids :: [Maybe Square] -> [Square]
valids msqrs = map fromJust $ filter isJust msqrs

at :: Char -> Int -> Board -> Maybe Square
at f r = findSquare (Square f r Nothing)

relative :: Square -> Int -> Board -> Direction -> Maybe Square
relative (Square f r c) n board dir = 
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

freeInDirection :: Square -> Direction -> Board -> [Square]
freeInDirection sqr dir board = 
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhile empty (reverse reverseSeq)
    where
        nth xs n = 
            case relative sqr n board dir of
                Just sqr -> sqr : xs
                Nothing -> [] ++ xs

inDirection :: Square ->Board -> Direction -> [Square]
inDirection sqr board dir = 
    let sqrs = freeInDirection sqr dir board
    in 
        if not (null sqrs) then
            next (head sqrs) dir board ++ sqrs
        else 
            []
    where
        next sqr dir board = maybeToList $ relative sqr 1 board dir

