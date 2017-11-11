module Main where

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Argonaut.Encode (encodeJson)

import Database.Postgres as PG

import Node.Express.App (App, get, listenHttp, setProp)
import Node.Express.Handler (Handler)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (EXPRESS, Event)
import Node.HTTP (HTTP, Server)
import Node.Process (PROCESS)

import Herigone.DB (selectAllAssociations)
import Herigone.Environment (getHttpServerPort)

associations :: forall e. Handler (db :: PG.DB | e)
associations = do
  queryResult <- liftAff selectAllAssociations
  setStatus 200
  sendJson $ encodeJson queryResult

app :: forall e. App (db :: PG.DB, http :: HTTP, console :: CONSOLE | e)
app = do
  setProp "json spaces" 2
  get "/api/v1/associations" associations

appStartCallback :: forall e. Event -> Eff (process :: PROCESS, console :: CONSOLE | e) Unit
appStartCallback event = do
  port <- liftEff getHttpServerPort
  log ("Express HTTP server listening on port " <> (show port) <> ".")

main :: forall e. Eff (express :: EXPRESS, console :: CONSOLE, db :: PG.DB, http :: HTTP, process :: PROCESS | e) Server
main = do
  port <- liftEff getHttpServerPort
  listenHttp app port appStartCallback
