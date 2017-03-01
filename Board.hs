
module Board where

import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as Map

import Direction
import Square
import Piece

type Map = Map.Map

data Board = Board { 
      _squares  :: [Square] 
    , _pieces   :: Map Square Piece
}

-------------------------------------------------------------------------------
-- display

instance Show Board where
    show board = 
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) [8,7..1] rsRep')
        where
            rs = reverse $ ranks board
            rsRep = concatMap (strRep board)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep board sqr =
                case occupiedBy sqr board of
                    Just pce -> "[" ++ show pce ++ "]"
                    Nothing  -> "[ ]"

ranks :: Board -> [[Square]]
ranks board = ranks' $ _squares board  
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

-------------------------------------------------------------------------------
-- piece inspection

occupied :: Board -> Square -> Bool
occupied board sqr = isJust $ Map.lookup sqr $ _pieces board

occupiedBy :: Square -> Board -> Maybe Piece
occupiedBy sqr board = Map.lookup sqr $ _pieces board

occupiedByColor :: Board -> Square -> Maybe Color
occupiedByColor board sqr = 
    case occupiedBy sqr board of
        Just sqr -> Just (_color sqr)
        Nothing  -> Nothing

notOccupiedBy :: Color -> Board -> [Square] -> [Square]
notOccupiedBy col brd = filter (\x -> Just col /= occupiedByColor brd x)

-------------------------------------------------------------------------------
-- get a single square

relative :: Int -> Square ->  Board -> Direction -> Maybe Square
relative n (Square f r) board dir = 
    boardSquare $ 
    let (f, r) = cords dir in Square f r
    where 
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n
        boardSquare sqr@(Square f r) 
            | f `elem` ['a'..'h'] && r `elem` [1..8] = Just sqr
            | otherwise                              = Nothing
        cords dir = 
            case dir of 
            North     -> (f    , nNorth)
            NorthEast -> (nEast, nNorth)
            East      -> (nEast, r     )
            SouthEast -> (nEast, nSouth)
            South     -> (f    , nSouth)
            SouthWest -> (nWest, nSouth)
            West      -> (nWest, r     )
            NorthWest -> (nWest, nNorth)

neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1

-------------------------------------------------------------------------------
-- get a sequence of squares

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

untilOccupied :: Square -> Board -> Direction -> [Square]
untilOccupied sqr board dir = 
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhileOneMore (not . occupied board) (reverse reverseSeq)
    where
        nth xs n = 
            case relative n sqr board dir of
                Just sqr -> sqr : xs
                Nothing -> [] ++ xs

-------------------------------------------------------------------------------
-- modify board

moveContent :: Square -> Square -> Board -> Board
moveContent sqr sqr' board =
    let 
        pce = occupiedBy sqr board
        pce' = occupiedBy sqr' board
        newmap = Map.delete sqr (_pieces board)
        newmap' = Map.delete sqr' newmap
        newmap'' = Map.insert sqr' (fromJust pce) newmap'
    in
        Board (_squares board) newmap''

