module Main where

import Prelude

import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile)
import Node.HTTP as H
import Node.Process (PROCESS)
import Node.Stream as S

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (encodeJson)

import Data.Either (Either, either)

import Database.Postgres as PG

import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)

import Data.Maybe as M

import Unsafe.Coerce (unsafeCoerce)

import Herigone.DB (selectAllAssociations)
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

respondToGET :: forall aff. H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE, fs :: FS | aff) Unit
respondToGET request response = do
  liftEff $ log "Responding to GET"
  let responseStream = H.responseAsStream response
      url            = H.requestURL request
  liftEff $ log ("Responding to GET to url " <> url)
  case url of
    -- TODO: Should be something like /api/v1/associations
    "/associations" -> do
      liftEff $ H.setStatusCode response 200
      liftEff $ H.setStatusMessage response "OK"
      liftEff $ H.setHeader response "Connection" "close"
      liftEff $ H.setHeader response "Transfer-Encoding" "identity"
      queryResult <- selectAllAssociations
      _ <- liftEff $ S.writeString responseStream UTF8 (stringify (encodeJson queryResult)) (pure unit)
      liftEff $ S.end responseStream (pure unit)
    "/index.html" -> do
      liftEff $ H.setStatusCode response 200
      liftEff $ H.setStatusMessage response "OK"
      liftEff $ H.setHeader response "Connection" "close"
      liftEff $ H.setHeader response "Transfer-Encoding" "identity"
      fileContent <- readTextFile UTF8 "static/index.html"
      _ <- liftEff $ S.writeString responseStream UTF8 fileContent (pure unit)
      liftEff $ S.end responseStream (pure unit)
    "/herigone.js" -> do
      liftEff $ H.setStatusCode response 200
      liftEff $ H.setStatusMessage response "OK"
      liftEff $ H.setHeader response "Connection" "close"
      liftEff $ H.setHeader response "Transfer-Encoding" "identity"
      fileContent <- readTextFile UTF8 "static/herigone.js"
      _ <- liftEff $ S.writeString responseStream UTF8 fileContent (pure unit)
      liftEff $ S.end responseStream (pure unit)
    _ -> do
      liftEff $ H.setStatusCode response 404
      liftEff $ H.setStatusMessage response "Not Found"
      liftEff $ H.setHeader response "Connection" "close"
      liftEff $ H.setHeader response "Transfer-Encoding" "identity"
      liftEff $ S.end responseStream (pure unit)

respondToUnsupportedMethod :: forall aff. H.Request -> H.Response -> Aff (http :: H.HTTP, console :: CONSOLE | aff) Unit
respondToUnsupportedMethod request response = do
  let responseStream = H.responseAsStream response
  liftEff $ H.setStatusCode response 405
  liftEff $ H.setStatusMessage response "Method Not Allowed"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ S.end responseStream (pure unit)

respond :: forall aff. H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE, fs :: FS | aff) Unit
respond request response = do
  let method = H.requestMethod request
      url    = H.requestURL request
  liftEff $ log ("Request with method: '" <> method <> "' to URL: '" <> url <> "'.")
  case method of
    "GET"  -> respondToGET request response
    _      -> respondToUnsupportedMethod request response

handleError :: forall eff. Error -> Eff eff Unit
handleError error = unsafeCoerce $ log ("ERROR: " <> show error)

handleSuccess :: forall eff a. a -> Eff eff Unit
handleSuccess value = pure unit

handleResponse :: forall eff a. Either Error a -> Eff eff Unit
handleResponse response = either handleError handleSuccess response

createServerFunction :: forall eff. H.Request -> H.Response -> Eff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE, fs :: FS | eff) Unit
createServerFunction request response =
  let canceler = runAff handleResponse (respond request response)
  in  map (const unit) canceler

asyncMain :: forall aff. Aff (db :: PG.DB, console :: CONSOLE, process :: PROCESS, http :: H.HTTP, fs :: FS | aff) Unit
asyncMain = do
  port <- liftEff getHttpServerPort
  server <- liftEff $ H.createServer createServerFunction
  liftEff $ H.listen server (getListenOptions port) (listenCallback port)
  pure unit

main :: forall eff. Eff (db :: PG.DB, exception :: EXCEPTION, console :: CONSOLE, process :: PROCESS, http :: H.HTTP, fs :: FS | eff) Unit
main = do
  canceler <- launchAff asyncMain
  pure unit
