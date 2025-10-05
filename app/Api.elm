module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import FatalError exposing (FatalError)
import Html exposing (Html)
import Route exposing (Route)
import Route.ChaserSixWhen
import View exposing (View)


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes {- getStaticRoutes -} _ htmlToString =
    [ chaserSixWhen htmlToString
    ]


chaserSixWhen :
    (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> ApiRoute ApiRoute.Response
chaserSixWhen htmlToString =
    ApiRoute.succeed
        (Route.ChaserSixWhen.data
            |> BackendTask.map
                (\data ->
                    Route.ChaserSixWhen.view { data = data } {}
                )
            |> BackendTask.andThen (toHtmlPage htmlToString)
        )
        |> ApiRoute.literal "chaser-six-when"
        |> ApiRoute.single


toHtmlPage :
    (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> View Never
    -> BackendTask FatalError String
toHtmlPage htmlToString view =
    BackendTask.map
        (\css ->
            let
                inner : String
                inner =
                    view.body
                        |> List.map (htmlToString Nothing)
                        |> String.concat
            in
            """<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><style>""" ++ css ++ """</style><title>Infinite Sea</title></head>
<body>
""" ++ inner ++ """
</body>
</html>"""
        )
        (File.rawFile "style.css" |> BackendTask.allowFatal)
