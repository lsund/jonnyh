
module JonnyH.Board where

import qualified Data.List           as List
import           Data.Map            (Map, elems, lookup)
import           GHC.Show
import           Prelude             ((!!))
import           Protolude           hiding (Map, evaluate, show)


import           JonnyH.Color
import           JonnyH.Direction
import           JonnyH.Piece.Common
import           JonnyH.Square
import           JonnyH.Util


type Position = Map Square Piece

type Move = (Square, Square)

data Board = Board {
      _squares  :: [Square]
    , _position :: Position
}

instance Eq Board where
    x == y = evaluate x == evaluate y

instance Ord Board where
    x `compare` y = evaluate x `compare` evaluate y


-------------------------------------------------------------------------------
-- Construct


board :: Position -> Board
board = Board [Square f r | f <- ['a'..'h'], r <- [1..8]]


evaluate :: Board -> Int
evaluate b = sumPces whites - sumPces blacks
    where
        (whites, blacks) = List.partition isWhite $ elems pos
        sumPces = foldl (\acc pce -> value pce + acc) 0 :: [Piece] -> Int
        pos = _position b


-------------------------------------------------------------------------------
-- Occupied?


occupied :: Square -> Board -> Bool
occupied sqr = isJust . lookup sqr . _position

pieceAt :: Square -> Board -> Maybe Piece
pieceAt sqr = lookup sqr . _position

colorAt :: Square -> Board -> Maybe Color
colorAt sqr b = _color <$> pieceAt sqr b

notOccupiedBy :: Color -> Board -> [Square] -> [Square]
notOccupiedBy col b = filter (\x -> Just col /= colorAt x b)

-------------------------------------------------------------------------------
-- Get a square


relative :: Int -> Square ->  Board -> Direction -> Maybe Square
relative n (Square f r) _ dir =
    boardSquare $
    let (f', r') = cords dir in Square f' r'
    where
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n
        boardSquare sqr@(Square f' r')
            | f' `elem` ['a'..'h'] && r' `elem` [1..8] = Just sqr
            | otherwise                              = Nothing
        cords dir' =
            case dir' of
            North     -> (f    , nNorth)
            NorthEast -> (nEast, nNorth)
            East      -> (nEast, r     )
            SouthEast -> (nEast, nSouth)
            South     -> (f    , nSouth)
            SouthWest -> (nWest, nSouth)
            West      -> (nWest, r     )
            NorthWest -> (nWest, nNorth)


-------------------------------------------------------------------------------
-- Neighbor


neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1


neighborIfOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfOccupied sqr b dir =
    mfilter (`occupied` b) (neighbor sqr b dir)


neighborIfNotOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfNotOccupied sqr b dir =
     mfilter (not . flip occupied b) (neighbor sqr b dir)


-- NOTE: Was originally reverse takeWhilePlus1
untilOccupied :: Square -> Board -> Direction -> [Square]
untilOccupied sqr b dir =
    let reverseSeq = foldl nth [] [1..8]
    in takeWhilePlus1 (not . flip occupied b) (reverse reverseSeq)
    where
        nth xs n =
            case relative n sqr b dir of
                Just s  -> s : xs
                Nothing -> [] ++ xs

-------------------------------------------------------------------------------
-- Display

instance Show Board where
    show b =
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) ([8,7..1] :: [Int]) rsRep')
        where
            rs = reverse $ ranks b
            rsRep = concatMap (strRep b)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep b' sqr =
                case pieceAt sqr b' of
                    Just pce -> "[" ++ show pce ++ "]"
                    Nothing  -> "[ ]"


ranks :: Board -> [[Square]]
ranks = ranks' . _squares
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

