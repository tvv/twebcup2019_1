module Models exposing (Model(..), toSession, isLoading, updateSession)

import Session exposing (Session)
import Url exposing (Url)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound


type Model
  = Root Url Session
  | NotFound Session
  | Home Home.Model
  | Login Login.Model


toSession : Model -> Session
toSession page =
  case page of

    Root _ session ->
      session

    NotFound session ->
      session

    Home home ->
      Home.toSession home

    Login login ->
      Login.toSession login


updateSession : Session -> Model -> Model
updateSession session page =
  case page of

    Root url _ ->
      Root url session

    NotFound _ ->
      NotFound session

    Home home ->
      Home.updateSession session home
        |> Home

    Login login ->
      Login.updateSession session login
        |> Login


isLoading : Model -> Bool 
isLoading page =
  case page of

    Root _ _ ->
      False

    NotFound _ ->
      False

    Home home ->
      Home.isLoading home

    Login login ->
      Login.isLoading login