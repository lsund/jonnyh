
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
        values = map moveToTuple $ filter (== Move{}) dat


-- mockGame = Game

moveMock = [Move 1 "d4" "b4", Move 2 "d4" "s5", GameResult "1" "0"]

mock = Metadata
        (Tag Event "Kongress")
        (Tag Site "Frankfurt")
        (Tag Date "1887")
        (Tag Round "13")
        (Tag White "Joseph")
        (Tag Black "Berger")
        (Tag Result "1-0")
        (Tag WhiteElo "")
        (Tag BlackElo "1242")
        (Tag ECO "D37")

rundb :: IO ()
rundb = do
    conn <- makeConnection
    _ <- insertMeta conn mock
    _ <- insertMoves conn moveMock
    return ()
    -- listContents conn
