module YaDisk exposing (..)

import Http
import Url
import Url.Builder as Builder
import Json.Decode as Decode exposing (Decoder)


type alias MetaRequestParams =
  { path: String
  , fields: Maybe String
  , limit: Int
  , offset: Int
  , previewCrop: Maybe Bool
  , previewSize: Maybe String
  , sort: Maybe String
  }


type Embedded = Embedded (List Resource)

type alias Resource =
  { id: String
  , name: String
  , t: String
  , embedded: Maybe ResourceList
  , mediaType: Maybe String
  , preview: Maybe String
  , publicUrl: Maybe String
  }


type alias ResourceList =
  { items: Embedded
  , limit: Int
  , offset: Int
  , path: String
  , sort: String
  }


decodeResource : Decoder Resource
decodeResource =
  Decode.map7 Resource
    (Decode.field "resource_id" Decode.string)
    (Decode.field "name" Decode.string)
    (Decode.field "type" Decode.string)
    (Decode.field "_embedded" decodeResourceList
      |> Decode.maybe)
    (Decode.field "media_type" Decode.string
      |> Decode.maybe)
    (Decode.field "preview" Decode.string
      |> Decode.maybe)
    (Decode.field "public_url" Decode.string
      |> Decode.maybe)


decodeResourceList : Decoder ResourceList
decodeResourceList =
  Decode.map5 ResourceList
    (Decode.field "items" 
      (Decode.map Embedded 
        (Decode.list 
          (Decode.lazy (\_ -> decodeResource))
        )
      )
    )
    (Decode.field "limit" Decode.int)
    (Decode.field "offset" Decode.int)
    (Decode.field "path" Decode.string)
    (Decode.field "sort" Decode.string)



getResourceMeta : (Result Http.Error Resource -> msg) -> String -> MetaRequestParams -> Cmd msg
getResourceMeta tag token params =
  let
    url = 
      Builder.crossOrigin 
        "https://cloud-api.yandex.net"
        ["v1", "disk", "resources"]
        (buildResourceMetaParams params)      
  in
    Http.request 
      { method = "GET"
      , headers = buildResourceMetaHeaders token
      , url = url
      , body = Http.emptyBody
      , expect = Http.expectJson tag decodeResource
      , timeout = Nothing
      , tracker = Nothing 
      }


buildResourceMetaParams : MetaRequestParams -> List Builder.QueryParameter 
buildResourceMetaParams params =
  (Builder.string "path" params.path)
  ::
  (Builder.int "limit" params.limit)
  ::
  (Builder.int "offset" params.offset)
  ::
  (params.fields
    |> Maybe.map (\v -> [Builder.string "fields" v])
    |> Maybe.withDefault [])
  ++
  (params.previewCrop
    |> Maybe.map (\v -> [Builder.string "preview_crop" (if v then "true" else "false")])
    |> Maybe.withDefault [])
  ++
  (params.previewSize
    |> Maybe.map (\v -> [Builder.string "preview_size" v])
    |> Maybe.withDefault [])
  ++
  (params.sort
    |> Maybe.map (\v -> [Builder.string "sort" v])
    |> Maybe.withDefault [])
      

buildResourceMetaHeaders : String -> List Http.Header
buildResourceMetaHeaders token =
  [ Http.header "Authorization" ("OAuth " ++ token)
  ]