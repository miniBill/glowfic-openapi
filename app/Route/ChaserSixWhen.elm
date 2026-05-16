module Route.ChaserSixWhen exposing (ActionData, Data, Model, Msg, RouteParams, data, route, view)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Extra
import GlowficApi.Types exposing (Character, Icon, PostDetails, Reply, User)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Html.Parser.Util
import List.Extra
import Pages.Url
import RouteBuilder exposing (App, StatelessRoute)
import Url
import UrlPath
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    Dict Int ( PostDetails, List Reply )


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
    Do.do GlowficApi.Extra.login <| \token ->
    go token [ rootPost ] Dict.empty


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
                    GlowficApi.Extra.getPost token h
                        |> BackendTask.andThen
                            (\( post, replies ) ->
                                let
                                    newIds =
                                        (post.content :: List.map .content replies)
                                            |> List.filterMap findLink
                                in
                                go token (newIds ++ t) (Dict.insert h ( post, replies ) acc)
                            )


view : { app | data : Data } -> Model -> View msg
view app _ =
    { title = "Chaser Six When?"
    , body =
        viewThread app.data rootPost
            |> List.singleton
            |> Html.div
                [ Html.Attributes.style "color" "#f3f3f3"
                , Html.Attributes.style "padding" "10px"
                , Html.Attributes.style "background" "#211e2f"
                ]
            |> List.singleton
    }


viewThread : Dict Int ( PostDetails, List Reply ) -> Int -> Html msg
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
                        -- Nothing ->
                        --     Html.text ""
                        -- Just description ->
                        description ->
                            Html.div
                                [ Html.Attributes.class "description" ]
                                [ Html.text description ]
                    ]
                    :: View.Post.viewPost post
                    :: viewReplies posts replies
                )


threadWidth : Dict Int ( PostDetails, List Reply ) -> List Reply -> Html.Attribute msg
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


parallels : Dict Int ( PostDetails, List Reply ) -> List Reply -> Int
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


viewReplies : Dict Int ( PostDetails, List Reply ) -> List Reply -> List (Html msg)
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
                            (View.Post.viewReply h :: viewReplies posts t)
                        , viewThread posts id
                        ]
                    ]

                Nothing ->
                    View.Post.viewReply h :: viewReplies posts t


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
