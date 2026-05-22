module Parser.Extra exposing (errorToHtml)

import Html exposing (Html)
import Html.Attributes
import Parser
import Parser.Error


errorToHtml :
    String
    -> List Parser.DeadEnd
    -> Html msg
errorToHtml src deadEnds =
    let
        color : String -> Html msg -> Html msg
        color value child =
            Html.span [ Html.Attributes.style "color" value ] [ child ]
    in
    Parser.Error.renderError
        { text = Html.text
        , formatContext = color "cyan"
        , formatCaret = color "red"
        , newline = Html.br [] []
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        src
        deadEnds
        |> Html.pre
            [ Html.Attributes.style "overflow" "scroll"
            , Html.Attributes.style "max-width" "calc(100vw-16px)"
            ]
