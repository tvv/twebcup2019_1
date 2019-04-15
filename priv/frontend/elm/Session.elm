module Session exposing (..)

import Http exposing (Progress(..))
import Browser.Navigation exposing (Key)
import File exposing (File)
import User exposing 
  ( Myself, User, myselfUser
  )

type alias Session = 
  { myself: Maybe Myself
  , navKey: Key
  , selectedFiles: List File
  , uploadingFile: Maybe UploadingFile
  }


type alias UploadingFile = 
  { file: File
  , progress: Int
  }


new : Maybe Myself -> Key -> Session
new myself navKey = 
  { myself = myself
  , navKey = navKey
  , selectedFiles = []
  , uploadingFile = Nothing
  }


getMyself : Session -> Maybe Myself
getMyself { myself } = myself


getNavKey : Session -> Key
getNavKey { navKey } = navKey


getUser : Session -> Maybe User
getUser session = 
  getMyself session
    |> Maybe.map myselfUser


getSelectedFiles : Session -> List File
getSelectedFiles { selectedFiles } = selectedFiles


getUploadingFile : Session -> Maybe UploadingFile
getUploadingFile { uploadingFile } = uploadingFile


isUploadingFile : Session -> Bool
isUploadingFile session =
  getUploadingFile session
    |> Maybe.map (\_ -> True)
    |> Maybe.withDefault False


setMyself : Maybe Myself -> Session -> Session
setMyself myself session = { session | myself = myself }


addSelectedFiles : List File -> Session -> Session
addSelectedFiles files session = 
  {session | selectedFiles = (getSelectedFiles session) ++ files}


setSelectedFiles : List File -> Session -> Session
setSelectedFiles files session = 
  {session | selectedFiles = files}


setUploadingFile : File -> Session -> Session
setUploadingFile file session = { session | uploadingFile = Just <| UploadingFile file 0 }


resetUploadingFile : Session -> Session
resetUploadingFile session = { session | uploadingFile = Nothing }


setUploadingFileProgress : Progress -> Session -> Session
setUploadingFileProgress progress session =
  getUploadingFile session
    |> Maybe.map (\uploadingFile ->
        case progress of
          Sending data ->
            {session 
            | uploadingFile = Just { uploadingFile | progress = round ( (Http.fractionSent data) * 100 )  }
            }

          _ ->
            session
      )
    |> Maybe.withDefault session


isAuthenticated : Session -> Bool
isAuthenticated session = 
  getUser session
    |> Maybe.map (\_ -> True)
    |> Maybe.withDefault False