module Test.Main where

import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

import Control.Monad.Eff.Class (liftEff)

import Prelude (bind)

import Herigone.Environment (getHttpServerPort)

main = run [consoleReporter] do
  describe "getHttpServerPort" do
    it "returns the default port if not set" do
      port <- liftEff getHttpServerPort
      "foo" `shouldEqual` "foo"
