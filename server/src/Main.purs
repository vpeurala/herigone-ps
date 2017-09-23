module Main where

import Node.Encoding (Encoding(UTF8))
import Node.HTTP as H
import Node.Stream as S
import Node.Process (PROCESS, lookupEnv)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Int as Int
import Data.Maybe as M

import Prelude

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

getListenOptions :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) H.ListenOptions
getListenOptions = do
  listeningPort <- getPort
  pure {
    hostname: "0.0.0.0",
    port: listeningPort,
    backlog: M.Nothing
  }

listenCallback :: forall eff. Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
listenCallback = log "HTTP server listening on port 9700."

respondToGET :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToGET request response = do
  let responseStream = H.responseAsStream response
      url            = H.requestURL request
  H.setStatusCode response 200
  H.setStatusMessage response "OK"
  H.setHeader response "Connection" "close"
  H.setHeader response "Transfer-Encoding" "identity"
  _ <- S.writeString responseStream UTF8 "Sook kook and die\n" (pure unit)
  S.end responseStream (pure unit)

respondToPOST :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToPOST request response = do
  respondToUnsupportedMethod request response

respondToUnsupportedMethod :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToUnsupportedMethod request response = do
  let responseStream = H.responseAsStream response
  H.setStatusCode response 405
  H.setStatusMessage response "Method Not Allowed"
  H.setHeader response "Connection" "close"
  S.end responseStream (pure unit)

respond :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respond request response = do
  let method = H.requestMethod request
      url    = H.requestURL request
  log ("Request with method: '" <> method <> "' to URL: '" <> url <> "'.")
  case method of
    "GET"  -> respondToGET request response
    "POST" -> respondToPOST request response
    _      -> respondToUnsupportedMethod request response

main :: forall eff. Eff (console :: CONSOLE, http :: H.HTTP, process :: PROCESS | eff) Unit
main = do
  log "Before creating server"
  server <- H.createServer respond
  listenOptions <- getListenOptions
  H.listen server listenOptions listenCallback
  log "Hello cockboys!"
