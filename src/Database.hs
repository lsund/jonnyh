
module Database where

import Protolude

import Database.PostgreSQL.Simple
import PGNParser


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }

listContents :: Connection -> IO ()
listContents conn =
    mapM_ print =<< ( query_ conn "select * from game" :: IO [(Int, Text)] )


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

insertMoves :: Connection -> [MoveText] -> IO Int64
insertMoves conn dat =
    executeMany conn q values
    where
        q = "insert into move (gameid, movenumber, white, black) values (?,?,?,?)"
        moveToTuple (Move n w b) = (1 :: Int, n, w, b)
        moveToTuple _            = (-1 :: Int, -1, "", "")
        values = map moveToTuple $ filter isMove dat

rundb :: IO ()
rundb = do
    conn <- makeConnection
    Right (Game meta moves : _) <- parseFile "resources/pgn/QGDOrthoMain.pgn"
    _ <- insertMeta conn meta
    _ <- insertMoves conn moves
    return ()
    -- listContents conn
