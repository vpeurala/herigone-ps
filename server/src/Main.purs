module Main where

import Node.Encoding (Encoding(UTF8))
import Node.HTTP as H
import Node.Stream as S
import Node.Process (PROCESS, lookupEnv)

import Database.Postgres as PG

import Control.Monad.Aff (Aff, launchAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, Error)

import Data.Foreign (F, Foreign, readInt, readString)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.Generic.Class (class GenericDecode)
import Data.Foreign.Generic.Types (Options)
import Data.Foreign.Index (readProp)
--import Data.Generic (class Generic)
import Data.Generic (class Generic, gShow)
import Data.Generic.Rep as Rep
--import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe as M
import Data.Show (class Show)

import Debug.Trace

import Prelude

data Association = Association {
  id :: Int,
  number :: String,
  word :: String
}

derive instance genericAssociation :: Generic Association

derive instance genericRepAssociation :: Rep.Generic Association _

instance showAssociation :: Show Association where
  show :: Association -> String
  show = gShow

instance associationDecode :: Decode Association where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

selectAllAssociations :: PG.Query Association
selectAllAssociations = PG.Query "SELECT id, number, word FROM association"

querySelectAllAssociations :: forall aff. PG.Client -> Aff (db :: PG.DB | aff) (Array Association)
querySelectAllAssociations dbClient = PG.query_ selectAllAssociations dbClient

databaseConnectionInfo :: PG.ConnectionInfo
databaseConnectionInfo = {
  host: "0.0.0.0",
  port: 5432,
  db: "herigone",
  user: "herigone",
  password: "herigone"
}

defaultPort :: Int
defaultPort = 9771

getPort :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Int
getPort = do
  environmentVariable <- lookupEnv "HERIGONE_SERVER_PORT"
  case environmentVariable of
    M.Nothing -> do
      log ("Environment variable HERIGONE_SERVER_PORT not set.")
      pure defaultPort
    M.Just environmentVariableValue -> do
      let parsedPort = Int.fromString environmentVariableValue
      case parsedPort of
        M.Nothing -> do
          log ("Environment variable HERIGONE_SERVER_PORT set to a value of \"" <> environmentVariableValue <> "\" which could not be parsed as an integer.")
          pure defaultPort
        M.Just portNumber -> do
          log ("Environment variable HERIGONE_SERVER_PORT set to a value of " <> (show portNumber) <> ".")
          pure portNumber

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

respondToGET :: forall aff. PG.Client -> H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | aff) Unit
respondToGET dbClient request response = do
  liftEff $ log "Responding to GET"
  let responseStream = H.responseAsStream response
      url            = H.requestURL request
  liftEff $ H.setStatusCode response 200
  liftEff $ H.setStatusMessage response "OK"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ H.setHeader response "Transfer-Encoding" "identity"
  liftEff $ log "Before DB query"
  queryResult <- querySelectAllAssociations dbClient
  liftEff $ log "After DB query"
  _ <- liftEff $ S.writeString responseStream UTF8 (show queryResult) (pure unit)
  liftEff $ S.end responseStream (pure unit)

respondToUnsupportedMethod :: forall aff. H.Request -> H.Response -> Aff (http :: H.HTTP, console :: CONSOLE | aff) Unit
respondToUnsupportedMethod request response = do
  let responseStream = H.responseAsStream response
  liftEff $ H.setStatusCode response 405
  liftEff $ H.setStatusMessage response "Method Not Allowed"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ S.end responseStream (pure unit)

respond :: forall aff. PG.Client -> H.Request -> H.Response -> Aff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | aff) Unit
respond dbClient request response = do
  let method = H.requestMethod request
      url    = H.requestURL request
  liftEff $ log ("Request with method: '" <> method <> "' to URL: '" <> url <> "'.")
  case method of
    "GET"  -> respondToGET dbClient request response
    _      -> respondToUnsupportedMethod request response

handleRespondError :: forall eff. Error -> Eff (console :: CONSOLE | eff) Unit
handleRespondError err =
  log ("Error: " <> show err)

handleRespondSuccess :: forall a eff. a -> Eff (console :: CONSOLE | eff) Unit
handleRespondSuccess succ =
  log ("Success!")

createServerFunction :: forall eff. PG.Client -> H.Request -> H.Response -> Eff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | eff) Unit
createServerFunction dbClient request response =
  let canceler = runAff handleRespondError handleRespondSuccess (respond dbClient request response)
  in  map (const unit) canceler

asyncMain :: forall aff. Aff ( db :: PG.DB, console :: CONSOLE, process :: PROCESS, http :: H.HTTP | aff) Unit
asyncMain = do
  dbClient <- PG.connect databaseConnectionInfo
  port <- liftEff getPort
  server <- liftEff $ H.createServer (createServerFunction dbClient)
  liftEff $ H.listen server (getListenOptions port) (listenCallback port)
  pure unit

main :: forall eff. Eff ( db :: PG.DB, exception :: EXCEPTION, console :: CONSOLE, process :: PROCESS, http :: H.HTTP | eff) Unit
main = do
  -- TODO: Maybe do something with the canceler returned by launchAff.
  _ <- launchAff asyncMain
  pure unit
