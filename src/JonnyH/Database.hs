
module JonnyH.Database where

import           Data.List                  (foldr1)
import           Data.Set                   hiding (filter, map)
import           Database.PostgreSQL.Simple
import           Protolude                  hiding (toList)

import           PGNParser.Data.Metadata
import           PGNParser.Data.MoveText
import           PGNParser.PGNParser


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }


insertMeta :: Connection -> Metadata -> IO Int64
insertMeta conn dat =
    execute conn q values
    where
        q =
            "insert into game \
            \(event, site, date, round, white, black, \
            \result, whiteelo, blackelo, eco) values \
            \(?,?,?,?,?,?,?,?,?,?)"
        values = ( (tagValue . _event) dat
                 , (tagValue . _site) dat
                 , (tagValue . _date) dat
                 , (tagValue . _round) dat
                 , (tagValue . _white) dat
                 , (tagValue . _black) dat
                 , (tagValue . _result) dat
                 , (tagValue . _whiteElo) dat
                 , (tagValue . _blackElo) dat
                 , (tagValue . _eco) dat
                 )

insertMoves :: Connection -> Int -> [MoveText] -> IO Int64
insertMoves conn i dat =
    executeMany conn q values
    where
        q = "insert into move (gameid, movenumber, white, black) values (?,?,?,?)"
        moveToTuple (Move n w b) = (i, n, w, b)
        moveToTuple _            = (-1 :: Int, -1, "", "")
        values = map moveToTuple $ filter isMove dat

maxId :: Connection -> IO Int
maxId conn = do
    (Only x : _) <- query_ conn "select max(id) from game" :: IO [Only Int]
    return x

insertGame :: Connection -> Game -> IO ()
insertGame conn (Game meta moves) = do
    _ <- insertMeta conn meta
    i <- maxId conn
    _ <- insertMoves conn i moves
    return ()

populate :: IO ()
populate = do
    conn <- makeConnection
    Right gs <- parseFile "resources/pgn/QGDOrthoMain.pgn"
    mapM_ (insertGame conn) gs
    return ()


queryMove :: Connection -> Query -> MoveText -> IO [Only Int]
queryMove conn q (Move n w b) = query conn q (n, w, b)
queryMove _ _ _ = return []

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

