module Main where

import Node.Encoding (Encoding(UTF8))
import Node.HTTP as H
import Node.Stream as S
import Node.Process (PROCESS)

import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

import Data.Maybe as M

import Database.Postgres as PG

import Prelude (Unit, bind, const, discard, map, pure, show, unit, ($), (<>))

import Herigone.DB (querySelectAllAssociations)
import Herigone.Environment (getHttpServerPort)

getListenOptions :: Int -> H.ListenOptions
getListenOptions listeningPort = {
    hostname: "0.0.0.0",
    port: listeningPort,
    backlog: M.Nothing
  }

listenCallback :: forall eff. Int -> Eff (console :: CONSOLE, http :: H.HTTP, process :: PROCESS | eff) Unit
listenCallback port = do
  log ("HTTP server listening on port " <> (show port) <> ".")
  pure unit

respondToGET :: forall aff. H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | aff) Unit
respondToGET request response = do
  liftEff $ log "Responding to GET"
  let responseStream = H.responseAsStream response
      url            = H.requestURL request
  liftEff $ H.setStatusCode response 200
  liftEff $ H.setStatusMessage response "OK"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ H.setHeader response "Transfer-Encoding" "identity"
  queryResult <- querySelectAllAssociations
  _ <- liftEff $ S.writeString responseStream UTF8 (stringify (encodeJson queryResult)) (pure unit)
  liftEff $ S.end responseStream (pure unit)

respondToUnsupportedMethod :: forall aff. H.Request -> H.Response -> Aff (http :: H.HTTP, console :: CONSOLE | aff) Unit
respondToUnsupportedMethod request response = do
  let responseStream = H.responseAsStream response
  liftEff $ H.setStatusCode response 405
  liftEff $ H.setStatusMessage response "Method Not Allowed"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ S.end responseStream (pure unit)

respond :: forall aff. H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | aff) Unit
respond request response = do
  let method = H.requestMethod request
      url    = H.requestURL request
  liftEff $ log ("Request with method: '" <> method <> "' to URL: '" <> url <> "'.")
  case method of
    "GET"  -> respondToGET request response
    _      -> respondToUnsupportedMethod request response

handleRespondError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
handleRespondError err =
  log ("Error: " <> show err)

handleRespondSuccess :: forall a eff. a -> Eff (console :: CONSOLE | eff) Unit
handleRespondSuccess succ =
  log ("Success!")

createServerFunction :: forall eff. H.Request -> H.Response -> Eff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | eff) Unit
createServerFunction request response =
  let canceler = runAff handleRespondError handleRespondSuccess (respond request response)
  in  map (const unit) canceler

asyncMain :: forall aff. Aff ( db :: PG.DB, console :: CONSOLE, process :: PROCESS, http :: H.HTTP | aff) Unit
asyncMain = do
  port <- liftEff getHttpServerPort
  server <- liftEff $ H.createServer createServerFunction
  liftEff $ H.listen server (getListenOptions port) (listenCallback port)
  pure unit

main :: forall eff. Eff ( db :: PG.DB, exception :: EXCEPTION, console :: CONSOLE, process :: PROCESS, http :: H.HTTP | eff) Unit
main = do
  -- TODO: Maybe do something with the canceler returned by launchAff.
  _ <- launchAff asyncMain
  pure unit
