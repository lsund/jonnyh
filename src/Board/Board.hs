
module Board.Board where

import              Prelude             ((!!))
import              GHC.Show
import              Protolude            hiding (Map, show, evaluate)
import              Data.Maybe
import qualified    Data.List as List
import qualified    Data.Map as Map

import              Color
import              Board.Direction
import              Util
import              Board.Square
import              Piece

type Map = Map.Map

type Position = Map Square Piece

data Board = Board {
      _squares  :: [Square]
    , _position :: Position
}

instance Eq Board where
    x == y = evaluate x == evaluate y

instance Ord Board where
    x `compare` y = evaluate x `compare` evaluate y

evaluate :: Board -> Int
evaluate brd = sumPces whites - sumPces blacks
    where
        (whites, blacks) = List.partition isWhite $ Map.elems pos
        sumPces = foldl (\acc pce -> value pce + acc) 0 :: [Piece] -> Int
        pos = _position brd

board :: Position -> Board
board = Board [Square f r | f <- ['a'..'h'], r <- [1..8]]

-------------------------------------------------------------------------------
-- display


instance Show Board where
    show b =
        "   A  B  C  D  E  F  G  H\n" ++
        concat (zipWith (\x ln -> show x ++ " " ++ ln) ([8,7..1] :: [Int]) rsRep')
        where
            rs = reverse $ ranks b
            rsRep = concatMap (strRep b)
            rsRep' = map ((++ "\n") . rsRep) rs
            strRep b' sqr =
                case occupiedBy sqr b' of
                    Just pce -> "[" ++ show pce ++ "]"
                    Nothing  -> "[ ]"

ranks :: Board -> [[Square]]
ranks = ranks' . _squares
    where
        ranks' sqrs       = map (`ofRank` sqrs) [1..8]
        ofRank r          = foldr (\s acc -> consIf (_rank s == r) s acc) []
        consIf True e acc = e : acc
        consIf _ _ acc    = acc

-------------------------------------------------------------------------------
-- piece inspection

occupied :: Board -> Square -> Bool
occupied b sqr = isJust $ Map.lookup sqr $ _position b

occupiedBy :: Square -> Board -> Maybe Piece
occupiedBy sqr = Map.lookup sqr . _position

occupiedByColor :: Board -> Square -> Maybe Color
occupiedByColor b sqr =
    case occupiedBy sqr b of
        Just s -> Just (_color s)
        Nothing  -> Nothing

notOccupiedBy :: Color -> Board -> [Square] -> [Square]
notOccupiedBy col brd = filter (\x -> Just col /= occupiedByColor brd x)

-------------------------------------------------------------------------------
-- get a single square

-- TODO
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

neighbor :: Square -> Board -> Direction -> Maybe Square
neighbor = relative 1

neighborIfOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfOccupied sqr brd dir =
    neighbor sqr brd dir >>= wrapIf (occupied brd)

neighborIfNotOccupied :: Square -> Board -> Direction -> Maybe Square
neighborIfNotOccupied sqr brd dir =
    neighbor sqr brd dir >>= wrapIf (not . occupied brd)

-------------------------------------------------------------------------------
-- get a sequence of squares

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x : ys else [x]) []

untilOccupied :: Square -> Board -> Direction -> [Square]
untilOccupied sqr b dir =
    let reverseSeq = foldl nth [] [1..8]
    in reverse $ takeWhileOneMore (not . occupied b) (reverse reverseSeq)
    where
        nth xs n =
            case relative n sqr b dir of
                Just s -> s : xs
                Nothing -> [] ++ xs

-------------------------------------------------------------------------------
-- modify board

move :: Square -> Board -> Square -> Board
move sqr brd sqr' =
    let
        pce = occupiedBy sqr brd
        newmap = Map.delete sqr (_position brd)
        newmap' = Map.delete sqr' newmap
        newmap'' = Map.insert sqr' (fromJust pce) newmap'
    in
        board newmap''

