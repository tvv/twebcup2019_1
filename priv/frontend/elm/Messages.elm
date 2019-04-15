module Messages exposing (Msg(..))

import Browser
import Http
import Url exposing (Url)
import User exposing (MyselfProtocol)
import Image exposing (ImageUploadProtocol)
import YaDisk exposing (Resource)
import Route exposing (Route)
import File exposing (File)
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound


type Msg
  = Ignored
  | Init (Result Http.Error MyselfProtocol)

  -- drag'n'drop
  | GotFilesMsg File (List File)

  -- image processing
  | GotFileSelectMsg
  | GotImageUploaded (Result Http.Error ImageUploadProtocol)
  | GotUploadProgress Http.Progress

  -- route events
  | ChangedRoute (Maybe Route)
  | ChangedUrl Url
  | ClickedLink Browser.UrlRequest

  -- nested component messages
  | GotHomeMsg Home.Msg
  | GotLoginMsg Login.Msg