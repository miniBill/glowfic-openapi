module Route.Timeline.Post.Id_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Annotation exposing (Annotation, MessageId(..))
import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, Reply)
import Head
import Head.Seo as Seo
import Html.Parser
import Id exposing (Id, PostId, ReplyId)
import List.Extra
import Monad exposing (Monad)
import Monad.Do as Do
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Rope exposing (Rope)
import RouteBuilder exposing (App, StatefulRoute)
import SeqDict exposing (SeqDict)
import Server.Response as Response exposing (Response)
import Shared
import UrlPath exposing (UrlPath)
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    ( PostDetails, List Reply )


type alias Model =
    { annotations : SeqDict (Id ReplyId) (List Annotation) }


type alias Msg =
    ()


type alias RouteParams =
    { id : String }


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.preRenderWithFallback
        { head = head
        , data = data
        , pages = BackendTask.succeed []
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init = init
            , subscriptions = subscriptions
            , update = update
            }


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect Msg )
init _ _ =
    ( { annotations = SeqDict.empty }, Effect.none )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update app _ msg model =
    case msg of
        () ->
            ( model, Effect.none )


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


data : RouteParams -> BackendTask FatalError (Response Data ErrorPage)
data params =
    Monad.run (monad params)


monad : RouteParams -> Monad (Response Data ErrorPage)
monad params =
    Do.do
        (case String.toInt params.id of
            Nothing ->
                ("Invalid id: " ++ params.id)
                    |> Monad.failString

            Just i ->
                let
                    boardId : Id PostId
                    boardId =
                        Id.unsafe i
                in
                Monad.succeed boardId
        )
    <| \postId ->
    Do.do (GlowficApi.Extra.getPost postId) <| \post ->
    Monad.succeed (Response.render post)


view : App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg ())
view app _ model =
    { title = (Tuple.first app.data).subject
    , body = [ View.Post.viewThread app.data ]
    }


calculatePostsLinks : List ( { a | id : Id PostId }, List { b | id : Id ReplyId } ) -> Result ( String, List Parser.DeadEnd ) (List MessageId)
calculatePostsLinks posts =
    let
        replyToPost : SeqDict (Id ReplyId) (Id PostId)
        replyToPost =
            posts
                |> List.concatMap
                    (\( p, rs ) ->
                        let
                            pid =
                                Id.for p
                        in
                        List.map (\r -> ( Id.for r, pid )) rs
                    )
                |> SeqDict.fromList
    in
    calculatePostsLinksHelper replyToPost []


calculatePostsLinksHelper : SeqDict (Id ReplyId) (Id PostId) -> List ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (List MessageId)
calculatePostsLinksHelper replyToPost posts =
    posts
        |> Result.Extra.combineMap (calculatePostLinks replyToPost)
        |> Result.map
            (\rs ->
                rs
                    |> Rope.fromRopeList
                    |> Rope.toList
            )


calculatePostLinks : SeqDict (Id ReplyId) (Id PostId) -> ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (Rope MessageId)
calculatePostLinks replyToPost ( post, replies ) =
    Result.map2 Rope.appendTo
        (calculatePostDetailsLinks replyToPost post)
        (Result.map Rope.fromRopeList (Result.Extra.combineMap (calculateReplyLinks replyToPost post) replies))


calculatePostDetailsLinks : SeqDict (Id ReplyId) (Id PostId) -> PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope MessageId)
calculatePostDetailsLinks replyToPost ({ content } as p) =
    calculateContentLinks replyToPost (MessageIdPost (Id.for p)) content


calculateReplyLinks : SeqDict (Id ReplyId) (Id PostId) -> PostDetails -> Reply -> Result ( String, List Parser.DeadEnd ) (Rope MessageId)
calculateReplyLinks replyToPost post reply =
    calculateContentLinks replyToPost (MessageIdReply (Id.for post) (Id.for reply)) reply.content


calculateContentLinks : SeqDict (Id ReplyId) (Id PostId) -> MessageId -> String -> Result ( String, List Parser.DeadEnd ) (Rope MessageId)
calculateContentLinks replyToPost from content =
    case Html.Parser.run Html.Parser.noCharRefs content of
        Err e ->
            Err ( content, e )

        Ok parsed ->
            parsed
                |> Result.Extra.combineMap (calculateNodeLinks replyToPost from)
                |> Result.map Rope.fromRopeList


calculateNodeLinks : SeqDict (Id ReplyId) (Id PostId) -> MessageId -> Html.Parser.Node -> Result ( String, List Parser.DeadEnd ) (Rope MessageId)
calculateNodeLinks replyToPost from node =
    case node of
        Html.Parser.Element "a" attrs children ->
            case List.Extra.find (\( attrName, _ ) -> attrName == "href") attrs of
                Just ( _, target ) ->
                    case Parser.run (targetParser replyToPost from) target of
                        Err e ->
                            Err ( target, e )

                        Ok Nothing ->
                            Ok Rope.empty

                        Ok (Just to) ->
                            Ok (Rope.singleton to)

                Nothing ->
                    children
                        |> Result.Extra.combineMap (calculateNodeLinks replyToPost from)
                        |> Result.map Rope.fromRopeList

        Html.Parser.Element _ _ children ->
            children
                |> Result.Extra.combineMap (calculateNodeLinks replyToPost from)
                |> Result.map Rope.fromRopeList

        Html.Parser.Text _ ->
            Ok Rope.empty

        Html.Parser.Comment _ ->
            Ok Rope.empty


targetParser : SeqDict (Id ReplyId) (Id PostId) -> MessageId -> Parser (Maybe MessageId)
targetParser replyToPost from =
    Parser.oneOf
        [ Parser.succeed Id.unsafe
            |. Parser.oneOf
                [ Parser.token "/replies/"
                , Parser.token "https://glowfic.com/replies/"
                ]
            |= Parser.int
            |. Parser.token "#reply-"
            |. Parser.int
            |> Parser.andThen
                (\id ->
                    case SeqDict.get id replyToPost of
                        Just pid ->
                            Parser.succeed (Just (MessageIdReply pid id))

                        Nothing ->
                            if from == MessageIdReply (Id.unsafe 58782) (Id.unsafe 2596296) then
                                Parser.succeed Nothing

                            else
                                ("While parsing " ++ Annotation.messageIdToString from ++ ", could not find post for reply id " ++ Id.toString id)
                                    |> Parser.problem
                )
        , Parser.succeed (\reply -> Just (MessageIdPost (Id.unsafe reply)))
            |. Parser.oneOf
                [ Parser.token "/posts/"
                , Parser.token "https://glowfic.com/posts/"
                ]
            |= Parser.int
        , Parser.succeed Nothing
            |. Parser.oneOf
                [ Parser.token "https://www.d20pfsrd.com/"
                , Parser.token "https://www.aonprd.com/"
                , Parser.token "https://en.wikipedia.org/"
                , Parser.token "https://www.willowandroxas.com/"
                ]
            |. Parser.chompWhile (\_ -> True)
        ]
        |. Parser.end
