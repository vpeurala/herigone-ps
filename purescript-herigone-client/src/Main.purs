module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Fiber, launchAff)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)

import Prelude

main :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE | eff) (Fiber (ajax :: AJAX, console :: CONSOLE | eff) Unit)
main = launchAff $ do
  res <- affjax $ defaultRequest { url = "/associations", method = Left GET }
  liftEff $ log $ "GET /associations response: " <> res.response
