module Main where

import Node.HTTP (Server(), createServer)

import Control.Bind (bind)
import Control.Monad.Eff.Console

main = do
  server <- createServer
  log "Hello cockboys!"

