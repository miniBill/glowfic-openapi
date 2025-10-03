module Api exposing (..)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import FatalError exposing (FatalError)
import Html exposing (Html)
import Route exposing (Route)
import Route.Index


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response)
routes {- getStaticRoutes -} _ htmlToString =
    [ result htmlToString ]


result :
    (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> ApiRoute ApiRoute.Response
result htmlToString =
    ApiRoute.succeed
        (BackendTask.map2
            (\data stylesheet ->
                let
                    inner =
                        (Route.Index.view
                            { data = data }
                            {}
                        ).body
                            |> List.map (htmlToString Nothing)
                            |> String.concat
                in
                """<!DOCTYPE html>
<html lang="en">
<head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><style>""" ++ stylesheet ++ """</style><title>Infinite Sea</title></head>
<body>
""" ++ inner ++ """
</body>
</html>"""
            )
            Route.Index.data
            (File.rawFile "style.css" |> BackendTask.allowFatal)
        )
        |> ApiRoute.literal "result"
        |> ApiRoute.single
