module Parser.Extra exposing (errorToHtml, errorToString)

import Ansi.Color
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


errorToString : String -> List Parser.DeadEnd -> String
errorToString src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
        , newline = "\n"
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        src
        deadEnds
        |> String.join "\n"
