
module JonnyH.Database where

import Protolude
import Database.PostgreSQL.Simple

import PGNParser.PGNParser
import PGNParser.Data.Metadata
import PGNParser.Data.MoveText


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

rundb :: IO ()
rundb = do
    conn <- makeConnection
    Right gs <- parseFile "resources/pgn/QGDOrthoMain.pgn"
    mapM_ (insertGame conn) gs
    return ()
