module Page.NotFound exposing (view)

import Html exposing (Html, div)


view : { title : String, body : Html msg }
view =
  { title = "Page Not Found"
  , body = div [] []
  }
