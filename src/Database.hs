
module Database where

import Protolude

import Database.PostgreSQL.Simple


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }

listContents :: Connection -> IO ()
listContents conn =
    mapM_ print =<< ( query_ conn "select * from content" :: IO [Only Text] )


addEntry :: Connection -> IO Int64
addEntry conn = do
    putStrLn ("Enter a word" :: Text)
    word <- getLine
    execute conn "insert into content (title) values (?)" $ Only word

rundb :: IO ()
rundb = do
    conn <- makeConnection
    _ <- addEntry conn
    listContents conn
