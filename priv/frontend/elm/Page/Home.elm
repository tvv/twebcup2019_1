module Page.Home exposing 
  ( Model, Msg, init, view, update, subscriptions, toSession, isLoading
  , updateSession )

import Html exposing (Html, Attribute, div, text, img, span)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Html.Lazy
import Http
import Json.Decode as Decode
import Session exposing (Session, getUser)
import User exposing (userToken)
import Image exposing 
  ( Image, ImageListProtocol, ImageListParams, getList )
import YaDisk exposing 
  ( Resource, MetaRequestParams, Embedded(..), getResourceMeta )


type alias Model =
  { session: Session
  , images: List Image
  , modal: Maybe Int
  , params: ImageListParams
  , loading: Bool
  }


type Msg 
  = Ignored

  -- yandex disk messages
  | GotImageList (Result Http.Error ImageListProtocol)
  | GetMoreImages

  | ShowModalImage Int
  | HideModal
  | PrevImage
  | NextImage


init : Session -> (Model, Cmd Msg)
init session =
  let
    model = initialModel session
  in   
    getImageList model


initialModel : Session -> Model
initialModel session =
  { session = session
  , images = []
  , modal = Nothing
  , params = 
    { limit = 100
    , offset = 0
    , sort = Nothing
    }
  , loading = False
  }


view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Home page"
  , body = viewBody model
  }


viewBody : Model -> Html Msg
viewBody model =
  div [ class "index" ] 
    [ viewModal model
    , Html.Lazy.lazy viewImagesList model
    , case model.loading of
        True ->
          div [ class "more_images" ]
            [ div [ class "d-flex justify-content-center" ]
                [ div [ class "spinner-border" ]
                  [ span [ class "sr-only"] [ text "Loading..."]
                  ]
                ]
            ]
        
        False ->
          div [ class "more_images", onClick GetMoreImages ] [ text "Загрузить еще"]
    ]


viewModal : Model -> Html Msg
viewModal { images, modal } =
  case modal of
    Nothing ->
      text ""

    Just idx ->
      case List.head <| List.drop idx images of
        Nothing ->
          text ""

        Just image ->
          div [ class "modal_image", onClick HideModal]
            [ div [ class "modal_image__image"]
                [ div [ class "modal_image__actions" ] 
                    [ div [ class "modal_image__left", stopOnClick PrevImage ] 
                        [ text "←" ]
                    , div [ class "modal_image__right", stopOnClick NextImage ] 
                        [ text "→" ]
                    ]
                , img [ src image.uri, stopOnClick NextImage ] [] 
                ]
            , div [ class "modal_image__bg" ] [ text "" ]
            ]


stopOnClick : Msg -> Attribute Msg
stopOnClick msg =
  Html.Events.custom "click" (Decode.succeed 
    { message = msg
    , stopPropagation = True
    , preventDefault = True
    })


viewImagesList : Model -> Html Msg
viewImagesList { images } =
  List.indexedMap (\idx { preview } -> 
    div [ class "images__image" ] 
      [ img [ src preview, onClick <| ShowModalImage idx ] [] 
      ]
  ) images
  |> div [ class "images" ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 

    GotImageList (Ok { data }) ->
      case data of
        Just images ->
          ({ model 
            | images = model.images ++ images
            , loading = False
            }, Cmd.none)

        Nothing ->
          ({ model | loading = False }, Cmd.none)

    GotImageList (Err reason) ->
      ({ model | loading = False}, Cmd.none)

    GetMoreImages ->
      case model.loading of
        True ->
          (model, Cmd.none)

        False ->
          let
            params = model.params 
            newModel =          
              { model | params = { params | offset = List.length model.images } }
          in          
            getImageList newModel

    ShowModalImage idx ->
      ({model | modal = Just idx}, Cmd.none)

    HideModal ->
      ({model | modal = Nothing}, Cmd.none)

    PrevImage ->
      case model.modal of
        Nothing ->
          (model, Cmd.none)

        Just 0 ->
          ({model | modal = Just <| List.length model.images - 1}, Cmd.none)

        Just idx ->
          ({model | modal = Just <| idx - 1}, Cmd.none)

    NextImage ->
      case model.modal of
        Nothing ->
          (model, Cmd.none)

        Just idx ->
          case idx == List.length model.images - 1 of
            True ->
              ({model | modal = Just 0}, Cmd.none)

            False ->
              ({model | modal = Just <| idx + 1}, Cmd.none)

    Ignored ->
      (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


toSession : Model -> Session
toSession { session } = session


updateSession : Session -> Model -> Model
updateSession session model = { model | session = session }


isLoading : Model -> Bool
isLoading { loading, images } = 
  case images of
    [] -> loading
    _  -> False


getImageList : Model -> (Model, Cmd Msg)
getImageList ({ params } as model) =
  ({ model | loading = True }, getList GotImageList params )