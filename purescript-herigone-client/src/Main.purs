module Main where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Aff (Fiber, launchAff)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Either (Either(..), fromRight)
import Data.HTTP.Method (Method(..))

import Network.HTTP.Affjax (AJAX, affjax, defaultRequest, get)
import Network.HTTP.Affjax.Response (class Respondable)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode)

import Partial.Unsafe (unsafePartial)

import Herigone.Domain

import Prelude

main :: forall eff. Eff (ajax :: AJAX, console :: CONSOLE | eff) (Fiber (ajax :: AJAX, console :: CONSOLE | eff) Unit)
main = launchAff $ do
  res <- getAssociations
  let either' = decodeAssociationsFromResponse res.response
  let associations = unsafePartial $ fromRight either'
  liftEff $ log $ "associations: " <> (show associations)
  pure unit

decodeAssociationsFromResponse :: Json -> Either String (Array Association)
decodeAssociationsFromResponse json = gDecodeJson json

getAssociations :: forall eff. Aff (ajax :: AJAX | eff) { status :: StatusCode,
                                                          headers :: Array ResponseHeader,
                                                          response :: Json }
getAssociations = get "/associations"
