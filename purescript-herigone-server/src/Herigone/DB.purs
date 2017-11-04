module Herigone.DB (querySelectAllAssociations) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Database.Postgres (DB)
import Database.Postgres as PG

import Herigone.Domain (Association)

querySelectAllAssociations :: forall aff. Aff (db :: PG.DB | aff) (Array Association)
querySelectAllAssociations = do
  dbClient <- databaseClient
  PG.query_ selectAllAssociations dbClient

selectAllAssociations :: PG.Query Association
selectAllAssociations = PG.Query "SELECT id, number, word FROM association"

databaseClient :: forall eff. Aff (db :: DB | eff) PG.Client
databaseClient = do
  pool <- liftEff createConnectionPool
  client <- PG.connect pool
  pure client

createConnectionPool :: forall eff. Eff (db :: DB | eff) PG.Pool
createConnectionPool = PG.mkPool databaseConnectionInfo

databaseConnectionInfo :: PG.ConnectionInfo
databaseConnectionInfo = PG.connectionInfoFromConfig databaseClientConfig connectionPoolConfig

databaseClientConfig :: PG.ClientConfig
databaseClientConfig = {
  host: "herigone-ps-db",
  port: 5432,
  database: "herigone",
  user: "herigone",
  password: "xwnk0cddsPGXKNps",
  ssl: false
}

connectionPoolConfig :: PG.PoolConfig
connectionPoolConfig = {
  connectionTimeoutMillis: 1000,
  idleTimeoutMillis: 30000,
  max: 10
}
