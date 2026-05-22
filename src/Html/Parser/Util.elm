module Html.Parser.Util exposing (toVirtualDom)

{-| Converts nodes to virtual dom nodes.
-}

import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Parser exposing (Node(..))


toVirtualDom : List Node -> List (Html msg)
toVirtualDom nodes =
    List.map toVirtualDomEach nodes


toVirtualDomEach : Node -> Html msg
toVirtualDomEach node =
    case node of
        Element name attrs children ->
            Html.node name (List.map toAttribute attrs) (toVirtualDom children)

        Text s ->
            Html.text s

        Comment _ ->
            Html.text ""


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    Html.Attributes.attribute name value
