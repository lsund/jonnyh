
module JonnyH.Database.Query where

import           Data.List                  (foldr1)
import           Data.Set                   hiding (map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)

import           JonnyH.Database.Common
import           JonnyH.Color
import           PGNParser.Data.MoveText

maxId :: Connection -> IO Int
maxId conn = do
    (Only x : _) <- query_ conn "select max(id) from game" :: IO [Only Int]
    return x

queryMove :: Connection -> Query -> MoveText -> IO [Only Int]
queryMove conn q (Move n w b) = query conn q (n, w, b)
queryMove _ _ _               = return []

gamesWithMove :: Connection -> MoveText -> IO (Set Int)
gamesWithMove conn move = do
    let q = "select gameid from move where movenumber=? and white=? and black=?"
    xs <- queryMove conn q move
    return $ fromList $ map fromOnly xs


gamesWithMoveSequence :: Connection -> [MoveText] -> IO (Set Int)
gamesWithMoveSequence conn moves = do
    games <- mapM (gamesWithMove conn) moves
    return $ foldr1 intersection games

response :: [MoveText] -> Color -> Int -> IO [(Text, Int)]
response moves color n = do
    conn <- makeConnection
    g <- gamesWithMoveSequence conn moves
    let q = case color of
                White ->"select white,count(white) \
                        \from move \
                        \where gameid in ? and movenumber=? group by white"
                Black ->"select black,count(black) \
                        \from move \
                        \where gameid in ? and movenumber=? group by black"
        params = (In (toList g), n)
    query conn q params :: IO [(Text, Int)]

