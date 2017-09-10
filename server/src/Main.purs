module Main where

import Node.HTTP (Server(), createServer)

import Partial.Unsafe (unsafeCrashWith)

import Control.Bind (bind, discard)
import Control.Monad.Eff.Console (log)

main = do
  log "Before creating server"
  server <- createServer (unsafeCrashWith "foo!")
  log "Hello cockboys!"

