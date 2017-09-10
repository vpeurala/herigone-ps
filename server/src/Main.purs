module Main where

import Node.HTTP (HTTP, Request, Response, createServer)

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Unit (Unit, unit)

respond :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
respond request response = pure unit

main :: Eff (console :: CONSOLE, http :: HTTP) Unit
main = do
  log "Before creating server"
  server <- createServer respond
  log "Hello cockboys!"
