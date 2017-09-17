module Main where

import Node.Encoding (Encoding(UTF8))
import Node.HTTP as H
import Node.Stream as S

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)

import Prelude

listenOptions :: H.ListenOptions
listenOptions = {
  hostname: "localhost",
  port: 9700,
  backlog: Nothing
}

listenCallback :: Eff (http :: H.HTTP, console :: CONSOLE) Unit
listenCallback = log "HTTP server listening on port 9700."

respondToGET :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToGET request response = do
  let responseStream = H.responseAsStream response
  H.setStatusCode response 200
  H.setStatusMessage response "OK"
  _ <- S.writeString responseStream UTF8 "Sook kook\n" (pure unit)
  S.end responseStream (pure unit)

respondToPOST :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToPOST request response = do
  let responseStream = H.responseAsStream response
  H.setStatusCode response 405
  S.end responseStream (pure unit)

respondToUnsupportedMethod :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respondToUnsupportedMethod request response = do
  let responseStream = H.responseAsStream response
  H.setStatusCode response 405
  S.end responseStream (pure unit)

respond :: forall eff. H.Request -> H.Response -> Eff (http :: H.HTTP, console :: CONSOLE | eff) Unit
respond request response = do
  let method = H.requestMethod request
  log ("Request with method: " <> method)
  case method of
    "GET"  -> respondToGET request response
    "POST" -> respondToPOST request response
    _      -> respondToUnsupportedMethod request response

main :: Eff (console :: CONSOLE, http :: H.HTTP) Unit
main = do
  log "Before creating server"
  server <- H.createServer respond
  H.listen server listenOptions listenCallback
  log "Hello cockboys!"
