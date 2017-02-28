
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

instance Show Board where
    show board = 
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) [8,7..1] rsRep')
        where
            rs = reverse $ ranks board
            rsRep = concatMap (strRep board)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep board sqr =
                case findPiece sqr board of
                    Just pce -> "[" ++ show pce ++ "]"
                    Nothing  -> "[ ]"

ranks :: Board -> [[Square]]
ranks board = ranks' $ _squares board  
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

hasPiece :: Board -> Square -> Bool
hasPiece board sqr = isJust $ Map.lookup sqr $ _pieces board

findSquare :: Square -> Maybe Square
findSquare sqr@(Square f r) 
    | f `elem` ['a'..'h'] && r `elem` [1..8] = Just sqr
    | otherwise                              = Nothing

findPiece :: Square -> Board -> Maybe Piece
findPiece sqr board = Map.lookup sqr $ _pieces board

inBoard :: [Maybe Square] -> [Square]
inBoard msqrs = map fromJust $ filter isJust msqrs

-- frees :: [Square] -> Board -> [Square]
-- frees sqrs board = filter (\sqr -> isNothing (findPiece sqr board)) sqrs

relative :: Int -> Square ->  Board -> Direction -> Maybe Square
relative n (Square f r) board dir = 
    case dir of 
        North     -> findSquare (Square f       nNorth)
        NorthEast -> findSquare (Square nEast   nNorth)
        East      -> findSquare (Square nEast   r     )
        SouthEast -> findSquare (Square nEast   nSouth)
        South     -> findSquare (Square f       nSouth)
        SouthWest -> findSquare (Square nWest   nSouth)
        West      -> findSquare (Square nWest   r     )
        NorthWest -> findSquare (Square nWest   nNorth)
    where 
        nNorth    = iterate succ r !! n
        nEast     = iterate succ f !! n
        nSouth    = iterate pred r !! n
        nWest     = iterate pred f !! n

neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1

untilOccupied :: Square -> Board -> Direction -> [Square]
untilOccupied sqr board dir = 
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhile (not . hasPiece board) (reverse reverseSeq)
    where
        nth xs n = 
            case relative n sqr board dir of
                Just sqr -> sqr : xs
                Nothing -> [] ++ xs

-- inDirection :: Square -> Board -> Direction -> [Square]
-- inDirection sqr board dir = 
--     let sqrs = untilOccupied sqr board dir
--     in 
--         if not (null sqrs) then
--             next (head sqrs) dir board ++ sqrs
--         else 
--             []
--     where
--         next sqr dir board = maybeToList $ neighbor sqr board dir

move :: Square -> Square -> Board -> Board
move sqr sqr' board =
    let 
        pce = findPiece sqr board
        pce' = findPiece sqr' board
        newmap = Map.delete sqr (_pieces board)
        newmap' = Map.delete sqr' newmap
        newmap'' = Map.insert sqr' (fromJust pce) newmap'
    in
        Board (_squares board) newmap''

