module Herigone.Environment (getHttpServerPort) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Data.Int as Int
import Data.Maybe as M

import Node.Process (PROCESS, lookupEnv)

getHttpServerPort :: forall eff. Eff (process :: PROCESS, console :: CONSOLE | eff) Int
getHttpServerPort = do
  environmentVariable <- lookupEnv environmentVariablePortNumber
  case environmentVariable of
    M.Nothing -> do
      log $ "Environment variable " <> environmentVariablePortNumber <> " not set. Using the default port of " <> show defaultPort <> "."
      pure defaultPort
    M.Just environmentVariableValue -> do
      let parsedPort = Int.fromString environmentVariableValue
      case parsedPort of
        M.Nothing -> do
          log $ "Environment variable " <> environmentVariablePortNumber <> " set to a value of \"" <> environmentVariableValue <> "\" which could not be parsed as an integer. Using the default port of " <> show defaultPort <> "."
          pure defaultPort
        M.Just portNumber -> do
          log $ "Environment variable " <> environmentVariablePortNumber <> " set to a value of " <> show portNumber <> "."
          pure portNumber

defaultPort :: Int
defaultPort = 9771

environmentVariablePortNumber :: String
environmentVariablePortNumber = "HERIGONE_SERVER_PORT"
