module Herigone.Domain where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Generic (class Generic, gShow)
import Data.Generic.Rep as Rep

import Prelude

data Association = Association {
  id :: Int,
  number :: String,
  word :: String
}

derive instance genericAssociation :: Generic Association

derive instance genericRepAssociation :: Rep.Generic Association _

instance showAssociation :: Show Association where
  show = gShow

instance decodeAssociation :: Decode Association where
  decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })

instance encodeJsonAssociation :: EncodeJson Association where
  encodeJson = gEncodeJson

instance decodeJsonAssociation :: DecodeJson Association where
  decodeJson = gDecodeJson
