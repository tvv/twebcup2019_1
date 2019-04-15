module Page.Login exposing 
  ( Model, Msg, init, view, update, subscriptions, toSession, isLoading
  , updateSession )

import Html exposing (Html, div, text, form, input, button, img, a)
import Html.Attributes exposing (class, type_, placeholder, src, href)
import Html.Events exposing (onInput, onSubmit, onClick)
import Http
import Regex
import Json.Decode as Decode exposing (Decoder)
import HttpModel
import Session exposing (Session)


type alias Model =
  { session: Session
  , messages: List String
  , form: Form
  , loading: Bool
  , yandexOAuthSettings: Maybe OAuthSettings
  }


type alias Form =
  { login: String
  }


type alias OAuthSettingsProtocol = HttpModel.Protocol OAuthSettings


type alias OAuthSettings =
  { uri: String
  }


type Msg 
  = GotYandexOAuthSettings (Result Http.Error OAuthSettingsProtocol)
  | DoLogin
  | LoginTyped String


init : Session -> (Model, Cmd Msg)
init session =
  let
    model = initialModel session
    cmd = yandexOAuthSettings
  in
    (model, cmd)


initialModel : Session -> Model
initialModel session =
  { session = session
  , messages = []
  , form = initialForm session
  , loading = True
  , yandexOAuthSettings = Nothing
  }


initialForm : Session -> Form
initialForm session =
  { login = ""
  }


view : Model -> { title : String, body : Html Msg }
view model =
  { title = "Login"
  , body = viewBody model
  }


viewBody : Model -> Html Msg
viewBody model =
  div [ class "login" ]    
    [ viewMessages model.messages
    , viewYandexOAuthButton model.yandexOAuthSettings
    ]


viewMessages : List String -> Html Msg
viewMessages messages =
  case messages of
    [] -> 
      text ""

    _ ->
      div [ class "messages"]
        (List.map viewMessage messages)


viewMessage : String -> Html Msg
viewMessage message =
  div [ class "alert alert-warning"]
    [ text message ]


viewYandexOAuthButton : Maybe OAuthSettings -> Html Msg
viewYandexOAuthButton settings =
  settings
    |> Maybe.map (\{ uri } ->
      div [ class "login__yandex" ]
      [ a [ href uri] 
        [img 
          [ src "https://yastatic.net/s3/lpc/1aab46c7-a4fa-4627-a4a4-d321e8fbf82d.svg"
          ] []
        ]
      ])
    |> Maybe.withDefault (text "")


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ form } as model) =
  case msg of
    GotYandexOAuthSettings (Ok response) ->
      ({model | yandexOAuthSettings = response.data, loading = False}, Cmd.none)

    GotYandexOAuthSettings (Err reason) ->
      ({model | loading = False}, Cmd.none)

    LoginTyped login ->
      ({model | form = { form | login = login }}, Cmd.none)

    DoLogin ->
      case isValidEmail model.form.login of
        False ->
          ({model | messages = ["Введите корректный Email адрес"]}, Cmd.none)

        True ->
          (model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


toSession : Model -> Session
toSession { session } = session


updateSession : Session -> Model -> Model
updateSession session model = { model | session = session }


isLoading : Model -> Bool
isLoading { loading } = loading


isValidEmail : String -> Bool
isValidEmail email =
  let
    regex =
      Regex.fromString "^.*@.*$"
        |> Maybe.withDefault Regex.never
  in
    Regex.contains regex email


yandexOAuthSettings : Cmd Msg
yandexOAuthSettings =
  Http.get
    { url = "/api/v1/oauth/yandex"
    , expect = Http.expectJson GotYandexOAuthSettings decodeyandexOAuthSettings
    }


decodeyandexOAuthSettings : Decoder OAuthSettingsProtocol
decodeyandexOAuthSettings = 
  Decode.map OAuthSettings
    (Decode.field "uri" Decode.string)
    |> HttpModel.decode
  





