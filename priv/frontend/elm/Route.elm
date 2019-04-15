module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation
import Html exposing (Attribute)
import Html.Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)



-- ROUTING


type Route
  = Home
  | Login
  | Logout


parser : Parser (Route -> a) a
parser =
  oneOf
    [ Parser.map Home Parser.top
    , Parser.map Login (s "login")
    , Parser.map Logout (s "logout")
    ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
  Html.Attributes.href (routeToString targetRoute)


replaceUrl : Browser.Navigation.Key -> Route -> Cmd msg
replaceUrl key route =
  Browser.Navigation.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
  { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> Parser.parse parser



-- INTERNAL


routeToString : Route -> String
routeToString page =
  let
    pieces =
      case page of
        Home ->
            []

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]
  in
    "/" ++ String.join "/" pieces