module Route.ChaserSixWhen exposing (ActionData, Data, Model, Msg, RouteParams, data, route, view)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Parser
import Id exposing (Id, PostId)
import List.Extra
import Monad exposing (Monad)
import OpenApi.Common
import Pages.Url
import RouteBuilder exposing (App, StatelessRoute)
import SeqDict exposing (SeqDict)
import UrlPath
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    SeqDict (Id PostId) ( PostDetails, List Reply )


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


rootPost : Id PostId
rootPost =
    Id.unsafe 47527


data : BackendTask FatalError Data
data =
    go [ rootPost ] SeqDict.empty
        |> Monad.run


go : List (Id PostId) -> Data -> Monad Data
go ids acc =
    case ids of
        [] ->
            Monad.succeed acc

        h :: t ->
            case SeqDict.get h acc of
                Just _ ->
                    go t acc

                Nothing ->
                    GlowficApi.Extra.getPost h
                        |> Monad.andThen
                            (\( post, replies ) ->
                                let
                                    newIds =
                                        (post.content :: List.map .content replies)
                                            |> List.filterMap findLink
                                in
                                go (newIds ++ t) (SeqDict.insert h ( post, replies ) acc)
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


viewThread : SeqDict (Id PostId) ( PostDetails, List Reply ) -> Id PostId -> Html msg
viewThread posts id =
    case SeqDict.get id posts of
        Nothing ->
            Html.text ("Post #" ++ Id.toString id ++ " not found")

        Just ( post, replies ) ->
            Html.div
                [ Html.Attributes.class "thread"
                , threadWidth posts replies
                ]
                (viewHeader post
                    :: View.Post.viewPost post
                    :: viewReplies posts replies
                )


viewHeader : PostDetails -> Html msg
viewHeader post =
    Html.div []
        [ Html.div
            [ Html.Attributes.class "subject" ]
            [ Html.text post.subject ]
        , case post.description of
            OpenApi.Common.Null ->
                Html.text ""

            OpenApi.Common.Present description ->
                Html.div
                    [ Html.Attributes.class "description" ]
                    [ Html.text description ]
        ]


threadWidth : SeqDict (Id PostId) ( PostDetails, List Reply ) -> List Reply -> Html.Attribute msg
threadWidth posts replies =
    let
        l : Int
        l =
            SeqDict.size posts

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


parallels : SeqDict (Id PostId) ( PostDetails, List Reply ) -> List Reply -> Int
parallels posts queue =
    case queue of
        [] ->
            1

        h :: t ->
            case findLink h.content of
                Nothing ->
                    parallels posts t

                Just id ->
                    case SeqDict.get id posts of
                        Nothing ->
                            1 + parallels posts t

                        Just ( _, replies ) ->
                            parallels posts replies + parallels posts t


viewReplies : SeqDict (Id PostId) ( PostDetails, List Reply ) -> List Reply -> List (Html msg)
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


findLink : String -> Maybe (Id PostId)
findLink content =
    case Html.Parser.run Html.Parser.allCharRefs content of
        Ok nodes ->
            List.Extra.findMap findPostLink nodes

        Err _ ->
            Nothing


findPostLink : Html.Parser.Node -> Maybe (Id PostId)
findPostLink node =
    case node of
        Html.Parser.Element name attrs children ->
            case name of
                "a" ->
                    List.Extra.findMap
                        (\( attrName, attrValue ) ->
                            if attrName == "href" && String.startsWith "https://glowfic.com/posts/" attrValue then
                                String.dropLeft (String.length "https://glowfic.com/posts/") attrValue
                                    |> String.toInt
                                    |> Maybe.map Id.unsafe

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
