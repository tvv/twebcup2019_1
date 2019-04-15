module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation
import Url exposing (Url)
import Http
import Html exposing (Html)
import Html.Attributes exposing (class)
import Json.Decode exposing (Value)
import File exposing (File)
import File.Select
import Session exposing 
  ( Session, isAuthenticated, isUploadingFile, setUploadingFile, getSelectedFiles
  , setSelectedFiles, resetUploadingFile, setUploadingFileProgress )
import User exposing 
  (Myself, MyselfProtocol, getMyself)
import Image exposing (ImageUploadProtocol)
import YaDisk
import Route exposing (Route)
import Messages exposing (Msg(..))
import Models exposing (Model(..), toSession, updateSession, isLoading)
import Page
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound


main : Program Value Model Msg
main =
  Browser.application 
    { init = init
    , onUrlChange = ChangedUrl
    , onUrlRequest = ClickedLink
    , subscriptions = subscriptions
    , update = update
    , view = view
    }


init : Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
  ( Root url (Session.new Nothing navKey)
  , getMyself Init
  )


view : Model -> Document Msg
view model =
  let
    viewPage toMsg page =
      let
        { title, body } = page
      in
        { title = title
        , body = [Page.view (toSession model) (Html.map toMsg body)]
        }
  in
    case isLoading model of
      False ->
        case model of

          Root _ _ ->
            Page.loading

          NotFound _ ->
            viewPage (\_ -> Ignored) NotFound.view

          Home home ->
            viewPage GotHomeMsg (Home.view home)

          Login login ->
            viewPage GotLoginMsg (Login.view login)

      True ->
        Page.loading


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
  let
    session =
      toSession model
  in
    case maybeRoute of

      Nothing ->
        ( NotFound session, Cmd.none )

      Just Route.Home ->
        Home.init session
          |> updateWith Home GotHomeMsg model

      Just Route.Login ->
        Login.init session
          |> updateWith Login GotLoginMsg model

      Just Route.Logout ->
        Login.init session
          |> updateWith Login GotLoginMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case ( msg, model ) of
    ( Ignored, _ ) ->
      ( model, Cmd.none )

    (Init result, Root url _) ->
      updateInit result url model

    ( ClickedLink urlRequest, _ ) ->
      case urlRequest of
        Browser.Internal url ->
          case url.fragment of
            Nothing ->
              ( model, Cmd.none )

            Just _ ->
              ( model
              , Browser.Navigation.pushUrl (Session.getNavKey (toSession model)) (Url.toString url)
              )

        Browser.External href ->
          ( model
          , Browser.Navigation.load href
          )

    ( ChangedUrl url, _ ) ->
      changeRouteTo (Route.fromUrl url) model

    ( ChangedRoute route, _ ) ->
      changeRouteTo route model

    ( GotLoginMsg subMsg, Login login ) ->
      Login.update subMsg login
        |> updateWith Login GotLoginMsg model

    ( GotHomeMsg subMsg, Home home ) ->
      Home.update subMsg home
        |> updateWith Home GotHomeMsg model

    ( GotFilesMsg file files, _) ->
      updateGotFiles file files model

    (GotFileSelectMsg, _) ->
      (model, File.Select.files validMimeTypes GotFilesMsg)

    (GotImageUploaded response, _) ->
      updateGotImageUploaded response model

    (GotUploadProgress progress, _) ->
      updateGotUploadProgress progress model

    ( _, _ ) ->
      ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
  ( toModel subModel
  , Cmd.map toMsg subCmd
  )


updateInit : Result Http.Error MyselfProtocol -> Url -> Model -> (Model, Cmd Msg)
updateInit result url model =
  case result of
    Ok { data } ->
      let
        session = Session.setMyself data (toSession model)
        navKey = Session.getNavKey session
      in
        case isAuthenticated session of
          True ->
            let
              cmd =
                case Route.fromUrl url of
                  Nothing ->
                    Route.replaceUrl navKey Route.Home
                  
                  Just route ->
                    Route.replaceUrl navKey route
            in
              (Root url session, cmd)

          False ->
            (Root url session, Route.replaceUrl navKey Route.Login)

    Err _ ->
      changeRouteTo (Just Route.Login) model


updateGotFiles : File -> List File -> Model -> (Model, Cmd Msg)
updateGotFiles file files model =  
  case filterFiles (file::files) of
    [] ->
      (model, Cmd.none)

    (first::rest) as validFiles ->
      let
        session = (Session.addSelectedFiles validFiles (toSession model))
      in  
        case isUploadingFile session of
          True ->
            (updateSession session model, Cmd.none)

          False ->
            let
              cmd = Image.upload GotImageUploaded first            
            in                
              (updateSession (setUploadingFile file session) model, cmd)


updateGotImageUploaded : Result Http.Error ImageUploadProtocol -> Model -> (Model, Cmd Msg)
updateGotImageUploaded response model =
  let
    session = toSession model      
  in                
    case getSelectedFiles session of
      next::rest ->
        ( updateSession (setSelectedFiles rest (setUploadingFile next session)) model
        , Image.upload GotImageUploaded next
        )

      [] ->
        ( updateSession (resetUploadingFile session) model
        , Cmd.none
        )


updateGotUploadProgress : Http.Progress -> Model -> (Model, Cmd Msg)
updateGotUploadProgress progress model =
  ( updateSession (setUploadingFileProgress progress (toSession model)) model
  , Cmd.none
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Http.track Image.uploadTracker GotUploadProgress
    , case model of
        Root _ _ ->
          Sub.none

        NotFound _ ->
          Sub.none

        Home home ->
          Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
          Sub.map GotLoginMsg (Login.subscriptions login)
    ]


validMimeTypes : List String 
validMimeTypes =
  [ "image/png"
  , "image/jpeg"
  ]


filterFiles : List File -> List File
filterFiles files =
  List.filter (\file ->
    List.member (File.mime file) validMimeTypes
    ) files

