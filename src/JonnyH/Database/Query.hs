module JonnyH.Database.Query where

import           Data.List                  (foldr1, last)
import           Data.Set                   hiding (map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)
import           System.Random

import           JonnyH.Color
import           JonnyH.Database.Common
import           JonnyH.Square
import           JonnyH.Util
import           PGNParser.Data.Move

type PGNMove =  PGNParser.Data.Move.Move

type Ply = Square

type Move = (Ply, Ply)

maxId :: Connection -> IO Int
maxId conn = do
    (Only x : _) <- query_ conn "select max(id) from game" :: IO [Only Int]
    return x

queryMove :: Connection -> Query -> PGNMove -> IO [Only Int]
queryMove conn q (Move n w b) = query conn q (n, w, b)
queryMove _ _ _               = return []

gamesWithMove :: Connection -> PGNMove -> IO (Set Int)
gamesWithMove conn move = do
    let q = "select gameid from move where movenumber=? and white=? and black=?"
    xs <- queryMove conn q move
    return $ fromList $ map fromOnly xs


gamesWithMoveSequence :: Connection -> [PGNMove] -> IO (Set Int)
gamesWithMoveSequence conn moves = do
    games <- mapM (gamesWithMove conn) moves
    return $ foldr1 intersection games

randomMove :: Double -> [(Text, Int)] -> Maybe (Text, Double)
randomMove _ [] = Nothing
randomMove r xs =
    case dropWhile (\(_, y) -> r > y) (applyToSnds normalize xs) of
        []    -> Nothing
        y : _ -> Just y

doQuery :: Connection -> Set Int -> Maybe Int -> Color -> IO (Maybe (Text, Double))
doQuery conn games moveNumber color = do
    let q = case color of
                White ->"select white,count(white) \
                        \from move \
                        \where gameid in ? and movenumber=? group by white"
                Black ->"select black,count(black) \
                        \from move \
                        \where gameid in ? and movenumber=? group by black"
        params = (In (toList games), moveNumber)
    res <- query conn q params :: IO [(Text, Int)]
    r <- randomRIO (0.0, 1.0) :: IO Double
    return $ randomMove r res

nextMoveNumber :: [PGNMove] -> Maybe Int
nextMoveNumber =  (succ . last  <$>) . maybeEmptyList . map moveNumber
    where moveNumber (Move n _ _) = n
          moveNumber _            = -1

response :: [PGNMove] -> Color -> IO (Maybe (Text, Double))
response moves color = do
    conn <- makeConnection
    games <- gamesWithMoveSequence conn moves
    doQuery conn games (nextMoveNumber moves) color
