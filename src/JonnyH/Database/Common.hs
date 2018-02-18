
module JonnyH.Database.Common where

import           Database.PostgreSQL.Simple

import           Protolude


makeConnection :: IO Connection
makeConnection = connect defaultConnectInfo { connectDatabase = "chess" }


