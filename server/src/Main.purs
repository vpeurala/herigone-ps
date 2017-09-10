module Main where

import Node.HTTP (HTTP, Server(), Request, Response, createServer)

import Partial.Unsafe (unsafeCrashWith)

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

import Data.Unit (Unit, unit)

import Unsafe.Coerce (unsafeCoerce)

respond :: forall eff. Request -> Response -> Eff (http :: HTTP | eff) Unit
respond request response = pure unit

main = do
  log "Before creating server"
  server <- createServer respond
  log "Hello cockboys!"

