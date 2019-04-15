module User exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Http
import HttpModel


type alias MyselfProtocol = HttpModel.Protocol Myself


type alias Myself =
  { user: User
  , token: String
  }


type alias User = 
  { id: Int
  , name: String
  , email: String
  , token: String
  }


myselfUser : Myself -> User
myselfUser { user } = user


myselfToken : Myself -> String
myselfToken { token } = token


userId : User -> Int
userId { id } = id


userName : User -> String
userName { name } = name


userToken : User -> String
userToken { token } = token


decodeMyselfProtocol : Decoder MyselfProtocol
decodeMyselfProtocol =
  HttpModel.decode decodeMyself


decodeMyself : Decoder Myself
decodeMyself =
  Decode.map2 Myself
    (Decode.field "user" decodeUser)
    (Decode.field "token" Decode.string)


decodeUser : Decoder User
decodeUser =
  Decode.map4 User
    (Decode.field "id" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "email" Decode.string)
    (Decode.field "token" Decode.string)


getMyself : (Result Http.Error MyselfProtocol -> msg) -> Cmd msg
getMyself tag =
  Http.get
    { url = "/api/v1/auth/myself"
    , expect = Http.expectJson tag decodeMyselfProtocol
    }