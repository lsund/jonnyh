
module JonnyH.Database.Query where

import           Data.List                  (foldr1)
import           Data.Set                   hiding (map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)

import JonnyH.Database.Common
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

dummy = [ Move 1 "d4" "d5"
        , Move 2 "c4" "e6"
        , Move 3 "Nc3" "Nf6"
        , Move 4 "Bg5" "Be7"
        , Move 5 "e3" "h6"
        , Move 6 "Bh4" "O-O"
        , Move 7 "Rc1" "b6"
        ]

-- assuming white to play
respond moves color n = do
    conn <- makeConnection
    g <- gamesWithMoveSequence conn dummy
    let q = "select white,count(white) \
            \from move \
            \where gameid in ? and movenumber=? group by white"
        params = (In (toList g), n)
    res <- query conn q params :: IO [(Text, Int)]
    print res

