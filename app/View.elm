module View exposing (View, map)

import Html exposing (Html)


type alias View msg =
    { title : String
    , body : List (Html msg)
    }


map : (a -> b) -> View a -> View b
map f v =
    { title = v.title
    , body = List.map (Html.map f) v.body
    }
