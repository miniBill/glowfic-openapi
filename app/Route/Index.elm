module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Json
import GlowficApi.Types exposing (Character, Icon, Post, Reply, User)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import Json.Encode
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import Url
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
    GlowficApi.Api.postsId
        { authorization = { authorization = token }
        , params = { id = id }
        }
        |> BackendTask.andThen
            (\post ->
                List.range 0 ((post.num_replies - 1) // 100)
                    |> List.map
                        (\page ->
                            GlowficApi.Api.postsIdReplies
                                { authorization = { authorization = token }
                                , params =
                                    { id = id
                                    , page = Just page
                                    , per_page = Just 100
                                    }
                                }
                        )
                    |> BackendTask.combine
                    |> BackendTask.map List.concat
                    |> BackendTask.map (\replies -> ( post, replies ))
            )
        |> BackendTask.allowFatal


view : App Data ActionData {} -> Model -> View (PagesMsg msg)
view app _ =
    { title = "Chaser Six When?"
    , body =
        app.data
            |> Dict.toList
            |> List.map viewThread
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "row"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "color" "#f3f3f3"
                , Html.Attributes.style "gap" "10px"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "background" "#211e2f"
                ]
            |> List.singleton
    }


viewThread : ( Int, ( Post, List Reply ) ) -> Html msg
viewThread ( id, ( post, replies ) ) =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "10px"
        , Html.Attributes.style "background" "#131937"
        ]
        (Html.div []
            [ Html.div
                [ Html.Attributes.style "background" "#0e0b1e"
                , Html.Attributes.style "color" "#9c9aa4"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "font-size" "1.125rem"
                ]
                [ Html.text post.subject ]
            , case post.description of
                Nothing ->
                    Html.text ""

                Just description ->
                    Html.div
                        [ Html.Attributes.style "background" "#484357"
                        , Html.Attributes.style "padding" "6px 10px"
                        ]
                        [ Html.text description ]
            ]
            :: viewPost post
            :: List.map viewReply replies
        )


viewPost : Post -> Html msg
viewPost post =
    Html.div
        [ Html.Attributes.style "background" "#31323b"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "gap" "10px"
        ]
        [ viewCharacter
            { character = post.character
            , icon = post.icon
            , user =
                post.authors
                    |> List.head
                    |> Maybe.withDefault
                        { id = -1
                        , username = ""
                        }
            }
        , Html.p [ Html.Attributes.style "flex" "1 0" ]
            (viewPermalink ("https://glowfic.com/posts/" ++ String.fromInt post.id)
                :: viewContent post
            )
        ]


viewReply : Reply -> Html msg
viewReply reply =
    Html.div
        [ Html.Attributes.style "background" "#31323b"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "padding" "10px"
        , Html.Attributes.style "gap" "10px"
        ]
        [ viewCharacter reply
        , Html.p [ Html.Attributes.style "flex" "1 0" ]
            (viewPermalink ("https://glowfic.com/replies/" ++ String.fromInt reply.id)
                :: viewContent reply
            )
        ]


viewPermalink : String -> Html msg
viewPermalink url =
    Html.a
        [ Html.Attributes.href url
        , Html.Attributes.style "padding" "4px 0 4px 4px"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "float" "right"
        ]
        [ Html.img
            [ Html.Attributes.src "https://dhtmoj33sf3e0.cloudfront.net/assets/icons/link-bb9df2e290558f33c20c21f4a2a85841eb4ccb1bd09f6266d3e80679f30ccf62.png" ]
            []
        ]


viewCharacter :
    { a
        | icon : Maybe Icon
        , character : Maybe Character
        , user : User
    }
    -> Html msg
viewCharacter reply =
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.class "character"
        ]
        [ viewPicture reply
        , viewNames reply
        ]


viewPicture : { a | icon : Maybe Icon } -> Html msg
viewPicture { icon } =
    case icon of
        Just { id, url } ->
            Html.a
                [ Html.Attributes.href ("https://glowfic.com/icons/" ++ String.fromInt id)
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "background" "#0e0b1e"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                , Html.Attributes.style "align-items" "center"
                ]
                [ Html.img
                    [ Html.Attributes.src (Url.toString url)
                    , Html.Attributes.style "width" "100px"
                    ]
                    []
                ]

        Nothing ->
            Html.text ""


viewNames : { r | character : Maybe Character, user : User } -> Html msg
viewNames reply =
    Html.div
        [ Html.Attributes.style "font-weight" "700"
        ]
        [ viewCharacterNames reply
        , Html.a
            [ Html.Attributes.href ("https://glowfic.com/users/" ++ String.fromInt reply.user.id)
            , Html.Attributes.style "padding" "2px 6px"
            , Html.Attributes.style "background" "#0e0b1e"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-items" "center"
            ]
            [ Html.p
                []
                [ Html.text reply.user.username ]
            ]
        ]


viewCharacterNames : { r | character : Maybe Character } -> Html msg
viewCharacterNames reply =
    case reply.character of
        Nothing ->
            Html.text ""

        Just character ->
            Html.div
                [ Html.Attributes.style "background" "#111842"
                , Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-direction" "column"
                , Html.Attributes.style "align-items" "center"
                ]
                [ Html.a
                    [ Html.Attributes.href ("https://glowfic.com/characters/" ++ String.fromInt character.id)
                    ]
                    [ Html.p
                        [ Html.Attributes.style "padding" "2px 6px" ]
                        [ Html.text character.name ]
                    ]
                , case character.screenname of
                    Nothing ->
                        Html.text ""

                    Just screenname ->
                        Html.p
                            [ Html.Attributes.style "padding" "2px 6px"
                            , Html.Attributes.style "color" "#9c9aa4"
                            ]
                            [ Html.text screenname ]
                ]


viewContent : { a | content : String } -> List (Html msg)
viewContent reply =
    case Html.Parser.run reply.content of
        Err _ ->
            [ Html.text reply.content ]

        Ok node ->
            Html.Parser.Util.toVirtualDom node
