module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

import DOM (DOM)

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Network.HTTP.Affjax (AJAX, get)
import Network.HTTP.ResponseHeader (ResponseHeader)
import Network.HTTP.StatusCode (StatusCode)

import Herigone.Domain (Association)

type State = String

data Query a = Populate String a

data Message = Populated String

render :: State -> H.ComponentHTML Query
render state = HH.div [] [ HH.text state ]

eval :: forall m. Query ~> H.ComponentDSL State Query Message m
eval query = case query of
      Populate str x -> do
        H.put str
        H.raise $ Populated str
        pure x

messageDiv :: forall m. H.Component HH.HTML Query Unit Message m
messageDiv =
  H.component { initialState: const ""
              , render: render
              , eval: eval
              , receiver: const Nothing }

main :: forall eff. Eff (avar :: AVAR, ref :: REF, exception :: EXCEPTION, dom :: DOM, ajax :: AJAX, console :: CONSOLE | eff) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI messageDiv unit body
  res <- getAssociations
  let either' = decodeAssociationsFromResponse res.response
  case either' of
    (Left err) -> liftEff $ log err
    (Right associations) -> do
      io.query $ H.action $ Populate (show associations)

decodeAssociationsFromResponse :: Json -> Either String (Array Association)
decodeAssociationsFromResponse = gDecodeJson

getAssociations :: forall eff. Aff (ajax :: AJAX | eff) { status :: StatusCode, headers :: Array ResponseHeader, response :: Json }
getAssociations = get "/api/v1/associations"
