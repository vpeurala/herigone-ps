module Main where

import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen, requestMethod, setStatusCode, setStatusMessage)

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(Nothing))
import Data.Unit (Unit, unit)

import Prelude (($), (<>))

listenOptions :: ListenOptions
listenOptions = {
  hostname: "localhost",
  port: 9700,
  backlog: Nothing
}

listenCallback :: Eff (http :: HTTP, console :: CONSOLE) Unit
listenCallback = log "Server listening on port 9700."

respond :: forall eff. Request -> Response -> Eff (http :: HTTP, console :: CONSOLE | eff) Unit
respond request response = do
  let method = requestMethod request
  log ("Request with method: " <> method)
  setStatusCode response 200
  setStatusMessage response "OK"
  pure unit

main :: Eff (console :: CONSOLE, http :: HTTP) Unit
main = do
  log "Before creating server"
  server <- createServer respond
  listen server listenOptions listenCallback
  log "Hello cockboys!"
