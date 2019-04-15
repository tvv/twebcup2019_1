module HttpModel exposing (..)

import Json.Decode as Decode exposing (Decoder)


type alias Protocol a =
  { success: Bool
  , message: Maybe String
  , errors: Maybe (List String)
  , data: Maybe a
  }


decode : Decoder a -> Decoder (Protocol a)
decode typeDecoder =
  Decode.map4 Protocol
    (Decode.field "success" Decode.bool)
    (Decode.field "message" Decode.string
      |> Decode.maybe)
    (Decode.field "errors" (Decode.list Decode.string)
      |> Decode.maybe)
    (Decode.field "data" typeDecoder
      |> Decode.maybe)