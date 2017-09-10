module Main where

import Node.HTTP (HTTP, ListenOptions, Request, Response, createServer, listen)

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Maybe (Maybe(Nothing))
import Data.Unit (Unit, unit)

listenOptions :: ListenOptions
listenOptions = {
  hostname: "localhost",
  port: 9700,
  backlog: Nothing
}

listenCallback :: Eff (http :: HTTP, console :: CONSOLE) Unit
listenCallback = log "Server listening on port 9700."

respond :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
respond request response = pure unit

main :: Eff (console :: CONSOLE, http :: HTTP) Unit
main = do
  log "Before creating server"
  server <- createServer respond
  listen server listenOptions listenCallback
  log "Hello cockboys!"
