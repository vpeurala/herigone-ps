module Test.Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)

import Node.Process (setEnv)

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (PROCESS, run)

import Herigone.Environment (getHttpServerPort)

main :: forall eff. Eff (console :: CONSOLE, avar :: AVAR, process :: PROCESS | eff) Unit
main = run [consoleReporter] do
  describe "Herigone.Environment PureScript module" do

    describe "Capturing console logs" do
      it "enables us to see what is logged" do
        liftEff turnOnConsoleLogCapturing
        liftEff $ log "foo"
        logs <- liftEff getCapturedConsoleLogs
        liftEff turnOffConsoleLogCapturing
        logs `shouldEqual` ["foo"]

    describe "Function getHttpServerPort" do
      it "returns the default port if not set" do
        liftEff turnOnConsoleLogCapturing
        port <- liftEff unsafeGetHttpServerPort
        port `shouldEqual` 9771
        logs <- liftEff getCapturedConsoleLogs
        liftEff turnOffConsoleLogCapturing
        logs `shouldEqual` ["Environment variable HERIGONE_SERVER_PORT not set. Using the default port of 9771."]

      it "returns the default port if env variable HERIGONE_SERVER_PORT cannot be parsed as int" do
        liftEff $ unsafeSetEnv "HERIGONE_SERVER_PORT" "foo"
        liftEff turnOnConsoleLogCapturing
        port <- liftEff unsafeGetHttpServerPort
        port `shouldEqual` 9771
        logs <- liftEff getCapturedConsoleLogs
        liftEff turnOffConsoleLogCapturing
        logs `shouldEqual` ["Environment variable HERIGONE_SERVER_PORT set to a value of \"foo\" which could not be parsed as an integer. Using the default port of 9771."]

      it "returns the port set in env variable HERIGONE_SERVER_PORT if it is valid" do
        liftEff $ unsafeSetEnv "HERIGONE_SERVER_PORT" "9772"
        liftEff turnOnConsoleLogCapturing
        port <- liftEff unsafeGetHttpServerPort
        port `shouldEqual` 9772
        logs <- liftEff getCapturedConsoleLogs
        liftEff turnOffConsoleLogCapturing
        logs `shouldEqual` ["Environment variable HERIGONE_SERVER_PORT set to a value of 9772."]

unsafeGetHttpServerPort :: forall eff. Eff eff Int
unsafeGetHttpServerPort = unsafeCoerceEff getHttpServerPort

unsafeSetEnv :: forall eff. String -> String -> Eff eff Unit
unsafeSetEnv key value = unsafeCoerceEff $ setEnv key value

foreign import turnOnConsoleLogCapturing :: forall eff. Eff (console :: CONSOLE | eff) Unit

foreign import getCapturedConsoleLogs :: forall eff. Eff (console :: CONSOLE | eff) (Array String)

foreign import turnOffConsoleLogCapturing :: forall eff. Eff (console :: CONSOLE | eff) Unit
