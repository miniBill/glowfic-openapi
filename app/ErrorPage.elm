module ErrorPage exposing (ErrorPage(..), Model, Msg, head, init, internalError, notFound, statusCode, update, view)

import Effect exposing (Effect)
import Head
import Html
import View exposing (View)


type alias Msg =
    Never


type alias Model =
    {}


type ErrorPage
    = NotFound
    | InternalError String


notFound : ErrorPage
notFound =
    NotFound


internalError : String -> ErrorPage
internalError =
    InternalError


view : ErrorPage -> Model -> View Msg
view error _ =
    { body =
        [ Html.p []
            [ case error of
                NotFound ->
                    Html.text "Page not found. Maybe try another URL?"

                InternalError string ->
                    Html.text ("Something went wrong.\n" ++ string)
            ]
        ]
    , title =
        case error of
            NotFound ->
                "Page Not Found"

            InternalError _ ->
                "Unexpected Error"
    }


init : ErrorPage -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )


update : ErrorPage -> Msg -> Model -> ( Model, Effect Msg )
update _ msg _ =
    never msg


head : ErrorPage -> List Head.Tag
head _ =
    []


statusCode : ErrorPage -> number
statusCode error =
    case error of
        NotFound ->
            404

        InternalError _ ->
            500
