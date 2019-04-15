module Page exposing (..)

import Html exposing (Html, div, text, a, img, span)
import Html.Attributes exposing (class, classList, style)
import Html.Events exposing (onClick)
import Browser exposing (Document)
import Json.Decode as Decode
import Json.Encode as Encode
import File exposing (File)
import Messages exposing (Msg(..))
import Session exposing 
  ( Session, getUser, getSelectedFiles, getUploadingFile )
import User exposing (User)
import Route


view : Session -> Html Msg -> Html Msg
view session body =
  div [ class "top" ]
    [ [ header session
      ] |> headerLayout
    , bodyLayout body
    ]


loading : Document Msg
loading =
  { title = "Loading"
  , body = 
    [ div [ class "top" ]
      [ div [ class "d-flex justify-content-center" ]
        [ div [ class "spinner-border" ]
          [ span [ class "sr-only"] [ text "Loading..."]
          ]
        ]
      ]
    ]
  }


header : Session -> Html Msg
header session =
  getUser session
    |> Maybe.map (userHeader session)
    |> Maybe.withDefault (anonymousHeader session)


userHeader : Session -> User -> Html Msg
userHeader session user =
  div [ class "header__grid"] 
    [ dropZone session
    , userInfo user
    ]


anonymousHeader : Session -> Html Msg
anonymousHeader session =
  div [ class "header__grid"] 
    [ div [ class "header__dropzone" ] []
    , div [] 
        [ text "Авторизуйтесь" ]
    ]


dropZone : Session -> Html Msg
dropZone session =
  let
    (body, uploading) = 
      case getUploadingFile session of
        Nothing    -> 
          ([], False)

        Just { file, progress } -> 
          ([dropZoneFile progress file], True)
  in      
    div [ class "header__dropzone", classList [("header__dropzone-uploading", uploading)], onClick GotFileSelectMsg ] 
      body


dropZoneFile : Int -> File -> Html Msg 
dropZoneFile progress file =
  div [ class "progress header__dropzone__file" ]
    [ div 
      [ class "progress-bar bg-info"
      , style "width" (String.fromInt progress ++ "%")
      ]
      [ div [ class "header__dropzone__file__name" ] [ text <| File.name file ]
      ]
    ]


userInfo : User -> Html Msg
userInfo user =
  div [ class "header__userinfo" ] 
    [ div [ class "header__userinfo__name" ] [ text user.name ]
    ]


bodyLayout : Html Msg -> Html Msg
bodyLayout body =
  div [ class "content" ] [ body ]


headerLayout : List (Html Msg) -> Html Msg
headerLayout body =
  div [ class "header" ] 
    [ div [ class "header__container" ] body ]


noBubbleAllDragEvents : List (Html.Attribute Msg)
noBubbleAllDragEvents =
  List.map
    (\eventName ->
      Decode.succeed 
        { message = Ignored
        , stopPropagation = True
        , preventDefault = True 
        } |> Html.Events.custom eventName
    )
    ["drag", "dragstart", "dragend", "dragover", "dragenter", "dragleave", "drop"]


onDrop : Html.Attribute Msg
onDrop =
  Decode.field "dataTransfer" (Decode.field "files" (Decode.list File.decoder))
    |> Decode.map (\files ->
        { message = 
          case files of 
            []          -> Ignored
            first::rest -> GotFilesMsg first rest
        , stopPropagation = True
        , preventDefault = True 
        }
      ) |> Html.Events.custom "drop"
