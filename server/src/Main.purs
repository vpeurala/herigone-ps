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
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foreign (F, Foreign)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe as M

import Prelude (Unit, bind, const, discard, map, pure, show, unit, ($), (<>))

newtype Association = Association {
  id :: Int,
  number :: String,
  word :: String
}

derive instance associationGeneric :: Generic Association _
instance associationDecode :: Decode Association where decode = decodeAssociation

decodeAssociation :: Foreign -> F Association
decodeAssociation f = genericDecode defaultOptions f

selectAllAssociations :: PG.Query Association
selectAllAssociations = PG.Query "SELECT * FROM association"

querySelectAllAssociations :: forall aff. PG.Client -> Aff (db :: PG.DB | aff) (Array Association)
querySelectAllAssociations dbClient = PG.query selectAllAssociations [] dbClient

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
  let responseStream = H.responseAsStream response
      url            = H.requestURL request
  liftEff $ H.setStatusCode response 200
  liftEff $ H.setStatusMessage response "OK"
  liftEff $ H.setHeader response "Connection" "close"
  liftEff $ H.setHeader response "Transfer-Encoding" "identity"
  -- queryResult <- querySelectAllAssociations dbClient
  _ <- liftEff $ S.writeString responseStream UTF8 "Sook kook and die\n" (pure unit)
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

createServerFunction :: forall eff. PG.Client -> H.Request -> H.Response -> Eff (db :: PG.DB, http :: H.HTTP, console :: CONSOLE | eff) Unit
createServerFunction dbClient request response =
  let canceler = runAff (\err -> pure unit) (\succ -> pure unit) (respond dbClient request response)
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
