module Test.Main where

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import Prelude (Unit, bind, discard, ($))

import Herigone.Environment (getHttpServerPort)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Capturing console logs" do
    it "works" do
      _ <- liftEff turnOnConsoleLogCapturing
      liftEff $ log "foo"
      logs <- liftEff getCapturedConsoleLogs
      logs `shouldEqual` ["foo"]
      liftEff turnOffConsoleLogCapturing

  describe "getHttpServerPort" do
    it "returns the default port if not set" do
      port <- liftEff getHttpServerPort
      port `shouldEqual` 9771

foreign import turnOnConsoleLogCapturing :: forall eff. Eff (console :: CONSOLE | eff) Unit

foreign import getCapturedConsoleLogs :: forall eff. Eff (console :: CONSOLE | eff) (Array String)

foreign import turnOffConsoleLogCapturing :: forall eff. Eff (console :: CONSOLE | eff) Unit
