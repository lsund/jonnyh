
module JonnyH.Database.Query where

import           Data.List                  (foldr1)
import           Data.Set                   hiding (map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)
import           System.Random

import           JonnyH.Database.Common
import           JonnyH.Color
import           JonnyH.Util
import           PGNParser.Data.Move

type PGNMove =  PGNParser.Data.Move.Move

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

response :: [PGNMove] -> Color -> Int -> IO [(Text, Double)]
response moves color moveNumber = do
    conn <- makeConnection
    g <- gamesWithMoveSequence conn moves
    let q = case color of
                White ->"select white,count(white) \
                        \from move \
                        \where gameid in ? and movenumber=? group by white"
                Black ->"select black,count(black) \
                        \from move \
                        \where gameid in ? and movenumber=? group by black"
        params = (In (toList g), moveNumber)
    res <- query conn q params :: IO [(Text, Int)]
    return $ applyToSnds normalize res

