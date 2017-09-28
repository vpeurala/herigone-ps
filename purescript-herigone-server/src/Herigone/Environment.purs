module Herigone.Environment (getHttpServerPort) where

import Data.Int as Int
import Node.Process (PROCESS, lookupEnv)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe as M

import Prelude (bind, discard, pure, show, ($), (<>))

getHttpServerPort :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Int
getHttpServerPort = do
  environmentVariable <- lookupEnv "HERIGONE_SERVER_PORT"
  case environmentVariable of
    M.Nothing -> do
      log $ "Environment variable HERIGONE_SERVER_PORT not set. Using the default port of " <> show defaultPort <> "."
      pure defaultPort
    M.Just environmentVariableValue -> do
      let parsedPort = Int.fromString environmentVariableValue
      case parsedPort of
        M.Nothing -> do
          log $ "Environment variable HERIGONE_SERVER_PORT set to a value of \"" <> environmentVariableValue <> "\" which could not be parsed as an integer. Using the default port of " <> show defaultPort <> "."
          pure defaultPort
        M.Just portNumber -> do
          log $ "Environment variable HERIGONE_SERVER_PORT set to a value of " <> show portNumber <> "."
          pure portNumber

defaultPort :: Int
defaultPort = 9771
