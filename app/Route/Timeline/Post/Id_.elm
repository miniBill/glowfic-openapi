module Route.Timeline.Post.Id_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Annotation exposing (Annotation(..), MessageId(..))
import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Parser
import Id exposing (Id, PostId, ReplyId)
import List.Extra
import Monad exposing (Monad)
import Monad.Do as Do
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
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
    { post : ( PostDetails, List Reply )
    , annotations : SeqDict MessageId (List Annotation)
    }


type alias Model =
    { annotations : SeqDict MessageId (List Annotation) }


type Msg
    = AnnotationsChanged MessageId (List Annotation)


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
init app _ =
    ( { annotations = app.data.annotations }, Effect.none )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update app _ msg model =
    case msg of
        AnnotationsChanged id [] ->
            ( { model | annotations = SeqDict.remove id model.annotations }, Effect.none )

        AnnotationsChanged id newAnnotations ->
            ( { model | annotations = SeqDict.insert id newAnnotations model.annotations }, Effect.none )


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
    Do.do (calculatePostAnnotations post) <| \annotations ->
    let
        result : Data
        result =
            { post = post
            , annotations =
                annotations
                    |> List.Extra.gatherEqualsBy Tuple.first
                    |> List.map (\( ( k, h ), t ) -> ( k, h :: List.map Tuple.second t ))
                    |> SeqDict.fromList
            }
    in
    Monad.succeed (Response.render result)


view : App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg Msg)
view app _ model =
    { title = (Tuple.first app.data.post).subject
    , body = [ Html.map PagesMsg.fromMsg (viewThread model app.data.post) ]
    }


viewThread : Model -> ( PostDetails, List Reply ) -> Html Msg
viewThread model ( post, replies ) =
    Html.div
        [ Html.Attributes.class "thread"
        , Html.Attributes.style "color" "#f3f3f3"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "background" "#211e2f"
        , Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "1fr auto"
        , Html.Attributes.style "gap" "0 8px"
        ]
        (View.Post.viewHeader [ Html.Attributes.style "grid-column" "1 / span 2" ] post
            :: View.Post.viewTopPost [] post
            :: viewAnnotations [] model (MessageIdPost post.id)
            :: List.concatMap
                (\reply ->
                    [ View.Post.viewReply [] reply
                    , viewAnnotations [] model (MessageIdReply reply.id)
                    ]
                )
                replies
        )


viewAnnotations : List (Attribute Msg) -> Model -> MessageId -> Html Msg
viewAnnotations attrs model messageId =
    let
        annotations : List Annotation
        annotations =
            SeqDict.get messageId model.annotations
                |> Maybe.withDefault []

        addButton : Html (List Annotation)
        addButton =
            Html.button
                [-- Html.Events.onClick (annotations ++ [ emptyAnnotation ])
                ]
                [ Html.text "➕" ]
    in
    (List.indexedMap
        (\i annotation ->
            viewAnnotation annotation
                |> Html.map (\newAnnotation -> List.Extra.setAt i newAnnotation annotations)
        )
        annotations
        ++ [ addButton ]
    )
        |> List.map (Html.map (AnnotationsChanged messageId))
        |> Html.div attrs


viewAnnotation : Annotation -> Html Annotation
viewAnnotation annotation =
    let
        idInt : Int
        idInt =
            case annotation of
                Enter id ->
                    Id.toInt id

                Exit id ->
                    Id.toInt id

                HappensBefore (MessageIdPost id) ->
                    Id.toInt id

                HappensBefore (MessageIdReply id) ->
                    Id.toInt id

                HappensAfter (MessageIdPost id) ->
                    Id.toInt id

                HappensAfter (MessageIdReply id) ->
                    Id.toInt id

        selectOptions : List ( String, Annotation )
        selectOptions =
            [ ( "Enter id", Enter (Id.unsafe idInt) )
            , ( "Exit id", Exit (Id.unsafe idInt) )
            , ( "Happens before post", HappensBefore (MessageIdPost (Id.unsafe idInt)) )
            , ( "Happens before reply", HappensBefore (MessageIdReply (Id.unsafe idInt)) )
            , ( "Happens after post", HappensAfter (MessageIdPost (Id.unsafe idInt)) )
            , ( "Happens after reply", HappensAfter (MessageIdReply (Id.unsafe idInt)) )
            ]

        changeId : String -> Annotation
        changeId newIdString =
            let
                newId : Int
                newId =
                    Maybe.withDefault idInt (String.toInt newIdString)
            in
            case annotation of
                Enter _ ->
                    Enter (Id.unsafe newId)

                Exit _ ->
                    Exit (Id.unsafe newId)

                HappensBefore (MessageIdPost _) ->
                    HappensBefore (MessageIdPost (Id.unsafe newId))

                HappensBefore (MessageIdReply _) ->
                    HappensBefore (MessageIdReply (Id.unsafe newId))

                HappensAfter (MessageIdPost _) ->
                    HappensAfter (MessageIdPost (Id.unsafe newId))

                HappensAfter (MessageIdReply _) ->
                    HappensAfter (MessageIdReply (Id.unsafe newId))
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "4px"
        ]
        [ selectOptions
            |> List.map
                (\( label, value ) ->
                    Html.option
                        [ Html.Attributes.selected (value == annotation)
                        , Html.Attributes.value label
                        ]
                        [ Html.text label ]
                )
            |> Html.select
                [ Html.Events.onInput
                    (\key ->
                        selectOptions
                            |> List.Extra.findMap
                                (\( k, v ) ->
                                    if k == key then
                                        Just v

                                    else
                                        Nothing
                                )
                            |> Maybe.withDefault annotation
                    )
                ]
        , Html.input
            [ Html.Events.onInput changeId
            , Html.Attributes.value (String.fromInt idInt)
            ]
            []
        ]


calculatePostAnnotations : ( PostDetails, List Reply ) -> Monad (List ( MessageId, Annotation ))
calculatePostAnnotations ( post, replies ) =
    let
        result =
            Result.map2 (\l r -> Rope.appendTo l r |> Rope.toList)
                (calculatePostDetailsAnnotations post)
                (Result.map Rope.fromRopeList (Result.Extra.combineMap (calculateReplyAnnotations post) replies))
    in
    case result of
        Ok o ->
            Monad.succeed o

        Err ( t, e ) ->
            Monad.failString (Parser.Extra.errorToString t e)


calculatePostDetailsAnnotations : PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculatePostDetailsAnnotations ({ content } as p) =
    calculateContentAnnotations (MessageIdPost (Id.for p)) content


calculateReplyAnnotations : PostDetails -> Reply -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculateReplyAnnotations post reply =
    calculateContentAnnotations (MessageIdReply (Id.for reply)) reply.content


calculateContentAnnotations : MessageId -> String -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculateContentAnnotations from content =
    case Html.Parser.run Html.Parser.noCharRefs content of
        Err e ->
            Err ( content, e )

        Ok parsed ->
            parsed
                |> Result.Extra.combineMap (calculateNodeLinks from)
                |> Result.map Rope.fromRopeList


calculateNodeLinks : MessageId -> Html.Parser.Node -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculateNodeLinks from node =
    case node of
        Html.Parser.Element "a" attrs children ->
            case List.Extra.find (\( attrName, _ ) -> attrName == "href") attrs of
                Just ( _, target ) ->
                    case Parser.run (targetParser from) target of
                        Err e ->
                            Err ( target, e )

                        Ok Nothing ->
                            Ok Rope.empty

                        Ok (Just to) ->
                            Ok (Rope.singleton ( from, HappensAfter to ))

                Nothing ->
                    children
                        |> Result.Extra.combineMap (calculateNodeLinks from)
                        |> Result.map Rope.fromRopeList

        Html.Parser.Element _ _ children ->
            children
                |> Result.Extra.combineMap (calculateNodeLinks from)
                |> Result.map Rope.fromRopeList

        Html.Parser.Text _ ->
            Ok Rope.empty

        Html.Parser.Comment _ ->
            Ok Rope.empty


targetParser : MessageId -> Parser (Maybe MessageId)
targetParser from =
    Parser.oneOf
        [ Parser.succeed (\reply -> Just (MessageIdReply (Id.unsafe reply)))
            |. Parser.oneOf
                [ Parser.token "/replies/"
                , Parser.token "https://glowfic.com/replies/"
                ]
            |= Parser.int
            |. Parser.token "#reply-"
            |. Parser.int
        , Parser.succeed (\post -> Just (MessageIdPost (Id.unsafe post)))
            |. Parser.oneOf
                [ Parser.token "/posts/"
                , Parser.token "https://glowfic.com/posts/"
                ]
            |= Parser.int
        , Parser.succeed Nothing
            |. Parser.oneOf
                [ Parser.token "https://en.wikipedia.org/"
                , Parser.token "https://www.aonprd.com/"
                , Parser.token "https://www.d20pfsrd.com/"
                , Parser.token "https://www.willowandroxas.com/"
                ]
            |. Parser.chompWhile (\_ -> True)
        ]
        |. Parser.end
