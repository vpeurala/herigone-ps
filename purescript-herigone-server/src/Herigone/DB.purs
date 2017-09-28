module Herigone.DB where

import Control.Monad.Aff (Aff)

import Database.Postgres as PG

import Prelude (bind)

import Herigone.Domain (Association)

databaseConnectionInfo :: PG.ConnectionInfo
databaseConnectionInfo = {
  host: "0.0.0.0",
  port: 5432,
  db: "herigone",
  user: "herigone",
  password: "herigone"
}

selectAllAssociations :: PG.Query Association
selectAllAssociations = PG.Query "SELECT id, number, word FROM association"

querySelectAllAssociations :: forall aff. Aff (db :: PG.DB | aff) (Array Association)
querySelectAllAssociations = do
  dbClient <- databaseClient
  PG.query_ selectAllAssociations dbClient

databaseClient :: forall aff. Aff (db :: PG.DB | aff) PG.Client
databaseClient = PG.connect databaseConnectionInfo
