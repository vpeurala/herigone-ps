module Main where

import Control.Monad.Aff (Aff, Fiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Either (Either(..))

import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode)

import Herigone.Domain (Association)

import Prelude

main :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE | eff) (Fiber (ajax :: AJAX, console :: CONSOLE | eff) Unit)
main = launchAff $ do
  res <- getAssociations
  let either' = decodeAssociationsFromResponse res.response
  case either' of
    (Left err) -> liftEff $ log err
    (Right associations) -> do
      liftEff $ log $ "associations: " <> (show associations)

decodeAssociationsFromResponse :: Json -> Either String (Array Association)
decodeAssociationsFromResponse json = gDecodeJson json

getAssociations :: forall eff. Aff (ajax :: AJAX | eff) { status :: StatusCode,
                                                          headers :: Array ResponseHeader,
                                                          response :: Json }
getAssociations = get "/api/v1/associations"
