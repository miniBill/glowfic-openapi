module Route.Index exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Types exposing (Character, Icon, Post, Reply, User)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import List.Extra
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


rootPost : Int
rootPost =
    47527


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
                go token [ rootPost ] Dict.empty
            )


go : { token : String } -> List Int -> Data -> BackendTask FatalError Data
go token ids acc =
    case ids of
        [] ->
            BackendTask.succeed acc

        h :: t ->
            case Dict.get h acc of
                Just _ ->
                    go token t acc

                Nothing ->
                    getPost token h
                        |> BackendTask.andThen
                            (\( post, replies ) ->
                                let
                                    newIds =
                                        (post.content :: List.map .content replies)
                                            |> List.filterMap findLink
                                in
                                go token (newIds ++ t) (Dict.insert h ( post, replies ) acc)
                            )


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
        viewThread app.data 47527
            |> List.singleton
            |> Html.div
                [ Html.Attributes.style "color" "#f3f3f3"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "background" "#211e2f"
                ]
            |> List.singleton
    }


viewThread : Dict Int ( Post, List Reply ) -> Int -> Html msg
viewThread posts id =
    case Dict.get id posts of
        Nothing ->
            Html.text ("Post #" ++ String.fromInt id ++ " not found")

        Just ( post, replies ) ->
            Html.div
                [ Html.Attributes.class "thread"
                , threadWidth posts replies
                ]
                (Html.div []
                    [ Html.div
                        [ Html.Attributes.class "subject" ]
                        [ Html.text post.subject ]
                    , case post.description of
                        Nothing ->
                            Html.text ""

                        Just description ->
                            Html.div
                                [ Html.Attributes.class "description" ]
                                [ Html.text description ]
                    ]
                    :: viewPost post
                    :: viewReplies posts replies
                )


threadWidth : Dict Int ( Post, List Reply ) -> List Reply -> Html.Attribute msg
threadWidth posts replies =
    let
        l : Int
        l =
            Dict.size posts

        p : Int
        p =
            parallels posts replies
    in
    Html.Attributes.style "width"
        ("calc((100vw - 10px) / "
            ++ String.fromInt l
            ++ " * "
            ++ String.fromInt p
            ++ " - 10px"
            ++ ")"
        )


parallels : Dict Int ( Post, List Reply ) -> List Reply -> Int
parallels posts queue =
    case queue of
        [] ->
            1

        h :: t ->
            case findLink h.content of
                Nothing ->
                    parallels posts t

                Just id ->
                    case Dict.get id posts of
                        Nothing ->
                            1 + parallels posts t

                        Just ( _, replies ) ->
                            parallels posts replies + parallels posts t


viewReplies : Dict Int ( Post, List Reply ) -> List Reply -> List (Html msg)
viewReplies posts replies =
    case replies of
        [] ->
            []

        h :: t ->
            case findLink h.content of
                Just id ->
                    [ Html.div
                        [ Html.Attributes.class "split" ]
                        [ Html.div
                            [ Html.Attributes.class "thread", threadWidth posts t ]
                            (viewReply h :: viewReplies posts t)
                        , viewThread posts id
                        ]
                    ]

                Nothing ->
                    viewReply h :: viewReplies posts t


findLink : String -> Maybe Int
findLink content =
    case Html.Parser.run content of
        Ok nodes ->
            List.Extra.findMap findPostLink nodes

        Err _ ->
            Nothing


findPostLink : Html.Parser.Node -> Maybe Int
findPostLink node =
    case node of
        Html.Parser.Element name attrs children ->
            case name of
                "a" ->
                    List.Extra.findMap
                        (\( attrName, attrValue ) ->
                            if attrName == "href" && String.startsWith "https://glowfic.com/posts/" attrValue then
                                String.toInt (String.dropLeft (String.length "https://glowfic.com/posts/") attrValue)

                            else
                                Nothing
                        )
                        attrs

                _ ->
                    List.Extra.findMap findPostLink children

        Html.Parser.Text _ ->
            Nothing

        Html.Parser.Comment _ ->
            Nothing


viewPost : Post -> Html msg
viewPost post =
    Html.div
        [ Html.Attributes.class "reply" ]
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
        , Html.p
            [ Html.Attributes.class "content" ]
            (viewPermalink ("https://glowfic.com/posts/" ++ String.fromInt post.id)
                :: viewContent post
            )
        ]


viewReply : Reply -> Html msg
viewReply reply =
    Html.div
        [ Html.Attributes.class "reply" ]
        [ viewCharacter reply
        , Html.p
            [ Html.Attributes.class "content" ]
            (viewPermalink ("https://glowfic.com/replies/" ++ String.fromInt reply.id)
                :: viewContent reply
            )
        ]


viewPermalink : String -> Html msg
viewPermalink url =
    Html.a
        [ Html.Attributes.href url
        , Html.Attributes.class "permalink"
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
        [ Html.Attributes.class "character" ]
        [ viewPicture reply
        , viewNames reply
        ]


viewPicture : { a | icon : Maybe Icon } -> Html msg
viewPicture { icon } =
    case icon of
        Just { id, url } ->
            Html.a
                [ Html.Attributes.class "icon"
                , Html.Attributes.href ("https://glowfic.com/icons/" ++ String.fromInt id)
                ]
                [ Html.img [ Html.Attributes.src (Url.toString url) ] []
                ]

        Nothing ->
            Html.text ""


viewNames : { r | character : Maybe Character, user : User } -> Html msg
viewNames reply =
    Html.div
        [ Html.Attributes.class "names"
        ]
        [ viewCharacterNames reply
        , Html.a
            [ Html.Attributes.href ("https://glowfic.com/users/" ++ String.fromInt reply.user.id)
            , Html.Attributes.class "username"
            ]
            [ Html.p [] [ Html.text reply.user.username ]
            ]
        ]


viewCharacterNames : { r | character : Maybe Character } -> Html msg
viewCharacterNames reply =
    case reply.character of
        Nothing ->
            Html.text ""

        Just character ->
            Html.div
                [ Html.Attributes.class "character-name" ]
                [ Html.a
                    [ Html.Attributes.href ("https://glowfic.com/characters/" ++ String.fromInt character.id)
                    ]
                    [ Html.p [] [ Html.text character.name ] ]
                , case character.screenname of
                    Nothing ->
                        Html.text ""

                    Just screenname ->
                        Html.p
                            [ Html.Attributes.class "screenname" ]
                            [ Html.text screenname ]
                ]


viewContent : { a | content : String } -> List (Html msg)
viewContent reply =
    case Html.Parser.run reply.content of
        Err _ ->
            [ Html.text reply.content ]

        Ok node ->
            Html.Parser.Util.toVirtualDom node
