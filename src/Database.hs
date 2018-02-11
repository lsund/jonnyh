
module Database where

import Protolude

import Database.PostgreSQL.Simple
import Control.Monad
import Control.Applicative


makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }

listContents conn =
    mapM_ print =<< ( query_ conn "select * from content" :: IO [Only Text] )


addEntry conn = do
    putStrLn ("Enter a word" :: Text)
    word <- getLine
    execute conn "insert into content (title) values (?)" $ Only word

rundb = do
    conn <- makeConnection
    addEntry conn
    listContents conn
