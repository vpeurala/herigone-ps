module Test.Main where

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)

import Prelude (Unit, bind, discard, pure)

import Herigone.Environment (getHttpServerPort)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "captureConsoleLogs" do
    pure captureConsoleLogs
  describe "getHttpServerPort" do
    it "returns the default port if not set" do
      port <- liftEff getHttpServerPort
      port `shouldEqual` 9771

foreign import captureConsoleLogs :: Unit
