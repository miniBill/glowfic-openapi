module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (Post, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import UrlPath
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    Dict Int ( Post, List Reply )


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    {}


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


head : App Data ActionData RouteParams -> List Head.Tag
head _ =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = [ "images", "icon-png.png" ] |> UrlPath.join |> Pages.Url.fromPath
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "Welcome to elm-pages!"
        , locale = Nothing
        , title = "elm-pages is running"
        }
        |> Seo.website


data : BackendTask FatalError Data
data =
    BackendTask.map2 Tuple.pair
        (Env.expect "username")
        (Env.expect "password")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\( username, password ) ->
                GlowficApi.Api.login
                    { body =
                        { username = username
                        , password = password
                        }
                    }
                    |> BackendTask.allowFatal
            )
        |> BackendTask.andThen
            (\token ->
                go token [ 47527 ] Dict.empty
            )


go : { token : String } -> List Int -> Data -> BackendTask FatalError Data
go token ids acc =
    case ids of
        [] ->
            BackendTask.succeed acc

        h :: t ->
            getPost token h
                |> BackendTask.andThen (\post -> go token t (Dict.insert h post acc))


getPost : { token : String } -> Int -> BackendTask FatalError ( Post, List Reply )
getPost { token } id =
    BackendTask.map2 Tuple.pair
        (GlowficApi.Api.postsId
            { authorization = { authorization = token }
            , params = { id = id }
            }
        )
        (GlowficApi.Api.postsIdReplies
            { authorization = { authorization = token }
            , params = { id = id }
            }
        )
        |> BackendTask.allowFatal


view : App Data ActionData {} -> Model -> View (PagesMsg msg)
view app _ =
    { title = "Chaser Six When?"
    , body =
        app.data
            |> Dict.toList
            |> List.map viewThread
    }


viewThread : ( Int, ( Post, List Reply ) ) -> Html msg
viewThread ( id, ( post, replies ) ) =
    Html.div [ Html.Attributes.style "border" "1px solid black" ]
        (Html.text ("Id: " ++ String.fromInt id)
            :: viewPost post
            :: List.map viewReply replies
        )


viewPost : Post -> Html msg
viewPost post =
    Html.pre [ Html.Attributes.style "border" "1px solid black" ]
        [ post
            |> GlowficApi.Json.encodePost
            |> Json.Encode.encode 4
            |> Html.text
        ]


viewReply : Reply -> Html msg
viewReply reply =
    Html.pre [ Html.Attributes.style "border" "1px solid black" ]
        [ reply
            |> GlowficApi.Json.encodeReply
            |> Json.Encode.encode 4
            |> Html.text
        ]
