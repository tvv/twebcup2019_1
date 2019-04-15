module Image exposing (..)

import Json.Decode as Decode exposing (Decoder)
import File exposing (File)
import Url.Builder
import Http
import HttpModel


type alias ImageUploadProtocol = HttpModel.Protocol Image


type alias ImageListProtocol = HttpModel.Protocol (List Image)


type alias Image =
  { uri : String
  , preview : String
  }


type alias ImageListParams =
  { limit : Int
  , offset : Int
  , sort : Maybe String
  }


getList : (Result Http.Error ImageListProtocol -> msg) ->  ImageListParams -> Cmd msg
getList tag params =
  let
    url = 
      Url.Builder.absolute
        [ "api", "v1", "images" ]
        ([ Url.Builder.int "limit" params.limit
        , Url.Builder.int "offset" params.offset
        ] ++ (
          params.sort
            |> Maybe.map (\sort ->
                [Url.Builder.string "sort" sort]
              )
            |> Maybe.withDefault []
        ) )
  in      
    Http.get
      { url = url
      , expect = Http.expectJson tag decodeImageListProtocol
      }


upload : (Result Http.Error ImageUploadProtocol -> msg) -> File -> Cmd msg
upload tag file =
  let
    body = 
      Http.multipartBody
        [ Http.filePart "file" file
        ]      
  in      
    Http.request
      { method = "POST"
      , url = "/api/v1/images"
      , body = body
      , headers = []
      , expect = Http.expectJson tag decodeImageUploadProtocol
      , timeout = Nothing
      , tracker = Just uploadTracker
      }


uploadTracker : String
uploadTracker = "image_uploading"


decodeImageUploadProtocol : Decoder ImageUploadProtocol
decodeImageUploadProtocol =
  HttpModel.decode decodeImage


decodeImageListProtocol : Decoder ImageListProtocol
decodeImageListProtocol =  
  HttpModel.decode (Decode.list decodeImage)


decodeImage : Decoder Image
decodeImage =
  Decode.map2 Image 
    (Decode.field "uri" Decode.string)
    (Decode.field "preview" Decode.string)