module Route.Timeline.Post.Id_ exposing (ActionData, Data, Model, Msg, RouteParams, route)

import Annotation exposing (Annotation(..), MessageId(..))
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Codec exposing (Codec)
import Effect exposing (Effect)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Glowfic.Utils
import GlowficApi.Extra
import GlowficApi.Types exposing (Character, PostDetails, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Html.Parser
import Id exposing (CharacterId, Id, PostId, ReplyId)
import Json.Decode
import List.Extra
import Maybe.Extra
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
import SeqSet exposing (SeqSet)
import Server.Response as Response exposing (Response)
import Shared
import String.Extra
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
    | SaveAnnotations


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

        SaveAnnotations ->
            let
                postId : Id PostId
                postId =
                    (Tuple.first app.data.post).id
            in
            ( model
            , Effect.saveFile
                { filename = Glowfic.Utils.postAnnotationsFilename postId
                , mime = "application/json"
                , content = Codec.encodeToString 2 Glowfic.Utils.annotationsCodec model.annotations
                }
            )


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
    Do.do
        (Glowfic.Utils.readAnnotationsFromFile postId
            |> Monad.lift
            |> Monad.andThen
                (\e ->
                    case e of
                        Nothing ->
                            Glowfic.Utils.calculatePostAnnotations post
                                |> Monad.fromResult

                        Just o ->
                            Monad.succeed o
                )
        )
    <| \annotations ->
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
    , body =
        [ Html.map PagesMsg.fromMsg (viewThread model app.data.post)
        , Html.button
            [ Html.Events.onClick (PagesMsg.fromMsg SaveAnnotations)
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "bottom" "18px"
            , Html.Attributes.style "left" "218px"
            ]
            [ Html.text "Save annotations" ]
        ]
    }


type alias State =
    { onStage : SeqSet (Id CharacterId) }


applyAnnotations : List Annotation -> State -> State
applyAnnotations annotations state =
    List.foldl
        (\annotation s ->
            case annotation of
                Enter i ->
                    { s | onStage = SeqSet.insert i s.onStage }

                Exit i ->
                    { s | onStage = SeqSet.remove i s.onStage }

                HappensBefore _ ->
                    s

                HappensAfter _ ->
                    s
        )
        state
        annotations


viewThread : Model -> ( PostDetails, List Reply ) -> Html Msg
viewThread model ( post, replies ) =
    let
        characters : SeqDict (Id CharacterId) (SeqSet String)
        characters =
            Glowfic.Utils.allCharactersIds ( post, replies )

        afterTopPost =
            case SeqDict.get (MessageIdPost post.id) model.annotations of
                Nothing ->
                    { onStage = SeqSet.empty }

                Just annotations ->
                    applyAnnotations annotations
                        { onStage = SeqSet.empty }

        ( finalState, messageData ) =
            List.foldl
                (\reply ( state, acc ) ->
                    let
                        newState : State
                        newState =
                            case SeqDict.get (MessageIdReply reply.id) model.annotations of
                                Nothing ->
                                    state

                                Just annotations ->
                                    applyAnnotations annotations state
                    in
                    ( newState, newState :: acc )
                )
                ( afterTopPost, [] )
                replies
                |> Tuple.mapSecond List.reverse

        viewReply : Reply -> State -> List (Html Msg)
        viewReply reply state =
            [ View.Post.viewReply [] reply
            , viewAnnotations [] model characters reply.character state (MessageIdReply reply.id)
            ]
    in
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
            :: viewAnnotations [] model characters post.character afterTopPost (MessageIdPost post.id)
            :: List.concat (List.map2 viewReply replies messageData)
            ++ (case post.status of
                    GlowficApi.Types.Status__Complete ->
                        [ Html.div
                            [ Html.Attributes.style "grid-column" "1 / span 2" ]
                            [ Html.text "Complete" ]
                        ]

                    GlowficApi.Types.Status__Abandoned ->
                        []

                    GlowficApi.Types.Status__Active ->
                        []
               )
            ++ (if SeqSet.isEmpty finalState.onStage then
                    []

                else
                    let
                        idToCharacterName : Id CharacterId -> String
                        idToCharacterName id =
                            SeqDict.get id characters
                                |> Maybe.andThen
                                    (\l ->
                                        l
                                            |> SeqSet.toList
                                            |> List.map String.trim
                                            |> List.Extra.removeWhen String.isEmpty
                                            |> String.join ", "
                                            |> String.Extra.nonEmpty
                                    )
                                |> Maybe.withDefault ("??? " ++ Id.toString id)

                        finalCharacters : List String
                        finalCharacters =
                            finalState.onStage
                                |> SeqSet.toList
                                |> List.map idToCharacterName
                    in
                    [ Html.div
                        [ Html.Attributes.style "grid-column" "1 / span 2" ]
                        [ Html.text "On stage at the end:"
                        , Html.ul []
                            (List.map
                                (\character -> Html.li [] [ Html.text character ])
                                finalCharacters
                            )
                        ]
                    ]
               )
        )


viewAnnotations :
    List (Attribute Msg)
    -> Model
    -> SeqDict (Id CharacterId) (SeqSet String)
    -> Maybe Character
    -> State
    -> MessageId
    -> Html Msg
viewAnnotations attrs model characters messageCharacter state messageId =
    let
        annotations : List Annotation
        annotations =
            SeqDict.get messageId model.annotations
                |> Maybe.withDefault []

        addButton : Html Msg
        addButton =
            Html.button
                [ Html.Events.onClick
                    (AnnotationsChanged messageId
                        (annotations ++ [ newAnnotation ])
                    )
                , Html.Attributes.style "align-self" "start"
                ]
                [ Html.text "➕" ]

        newAnnotation : Annotation
        newAnnotation =
            let
                characterId : Id CharacterId
                characterId =
                    messageCharacter
                        |> Maybe.map .id
                        |> Maybe.withDefault
                            (Id.unsafe 0)
            in
            if SeqSet.member characterId state.onStage then
                Exit characterId

            else
                Enter characterId

        annotationViews : List (Html Msg)
        annotationViews =
            annotations
                |> List.indexedMap
                    (\i annotation ->
                        Html.div
                            [ Html.Attributes.style "display" "flex"
                            , Html.Attributes.style "gap" "4px"
                            ]
                            [ viewAnnotation characters annotation
                                |> Html.map
                                    (\changedAnnotation ->
                                        AnnotationsChanged messageId (List.Extra.setAt i changedAnnotation annotations)
                                    )
                            , Html.button
                                [ Html.Events.onClick
                                    (AnnotationsChanged messageId (List.Extra.removeAt i annotations))
                                ]
                                [ Html.text "🗑️" ]
                            ]
                    )

        warnings =
            case messageCharacter of
                Just character ->
                    if SeqSet.member character.id state.onStage || List.member (Exit character.id) annotations then
                        Html.text ""

                    else
                        Html.span
                            [ Html.Attributes.style "color" "red"
                            , Html.Attributes.style "font-weight" "bold"
                            ]
                            [ Html.text "Character missing Enter annotation" ]

                Nothing ->
                    Html.text ""
    in
    (annotationViews ++ [ addButton, warnings ])
        |> Html.div
            (Html.Attributes.style "display" "flex"
                :: Html.Attributes.style "flex-direction" "column"
                :: Html.Attributes.style "gap" "4px"
                :: attrs
            )


viewAnnotation : SeqDict (Id CharacterId) (SeqSet String) -> Annotation -> Html Annotation
viewAnnotation characters annotation =
    let
        none :
            { idString : String
            , characterIdMaybe : Maybe (Id CharacterId)
            , postIdMaybe : Maybe (Id PostId)
            , replyIdMaybe : Maybe (Id ReplyId)
            }
        none =
            { idString = ""
            , characterIdMaybe = Nothing
            , postIdMaybe = Nothing
            , replyIdMaybe = Nothing
            }

        ids :
            { idString : String
            , characterIdMaybe : Maybe (Id CharacterId)
            , postIdMaybe : Maybe (Id PostId)
            , replyIdMaybe : Maybe (Id ReplyId)
            }
        ids =
            case annotation of
                Enter id ->
                    { none | idString = Id.toString id, characterIdMaybe = Just id }

                Exit id ->
                    { none | idString = Id.toString id, characterIdMaybe = Just id }

                HappensBefore (MessageIdPost id) ->
                    { none | idString = Id.toString id, postIdMaybe = Just id }

                HappensBefore (MessageIdReply id) ->
                    { none | idString = Id.toString id, replyIdMaybe = Just id }

                HappensAfter (MessageIdPost id) ->
                    { none | idString = Id.toString id, postIdMaybe = Just id }

                HappensAfter (MessageIdReply id) ->
                    { none | idString = Id.toString id, replyIdMaybe = Just id }

        characterId : Id CharacterId
        characterId =
            ids.characterIdMaybe
                |> Maybe.Extra.orElse (List.head (SeqDict.keys characters))
                |> Maybe.withDefault (Id.unsafe 0)

        postId : Id PostId
        postId =
            ids.postIdMaybe |> Maybe.withDefault (Id.unsafe 0)

        replyId : Id ReplyId
        replyId =
            ids.replyIdMaybe |> Maybe.withDefault (Id.unsafe 0)

        selectOptions : List ( String, Annotation )
        selectOptions =
            [ ( "Enter", Enter characterId )
            , ( "Exit", Exit characterId )
            , ( "Happens before post", HappensBefore (MessageIdPost postId) )
            , ( "Happens before reply", HappensBefore (MessageIdReply replyId) )
            , ( "Happens after post", HappensAfter (MessageIdPost postId) )
            , ( "Happens after reply", HappensAfter (MessageIdReply replyId) )
            ]

        changeId : String -> Annotation
        changeId newIdString =
            let
                newId : Maybe Int
                newId =
                    -- Allow posting a full link, just grab the digits at the end
                    newIdString
                        |> String.toList
                        |> List.reverse
                        |> List.Extra.takeWhile Char.isDigit
                        |> List.reverse
                        |> String.fromList
                        |> String.toInt
            in
            case annotation of
                Enter oldId ->
                    Enter (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId)

                Exit oldId ->
                    Exit (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId)

                HappensBefore (MessageIdPost oldId) ->
                    HappensBefore (MessageIdPost (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId))

                HappensBefore (MessageIdReply oldId) ->
                    HappensBefore (MessageIdReply (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId))

                HappensAfter (MessageIdPost oldId) ->
                    HappensAfter (MessageIdPost (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId))

                HappensAfter (MessageIdReply oldId) ->
                    HappensAfter (MessageIdReply (newId |> Maybe.map Id.unsafe |> Maybe.withDefault oldId))

        characterSelect : (Id CharacterId -> Annotation) -> Html Annotation
        characterSelect ctor =
            let
                options : List ( String, Annotation )
                options =
                    characters
                        |> SeqDict.toList
                        |> List.map
                            (\( id, names ) ->
                                ( String.join ", " (SeqSet.toList names)
                                , ctor id
                                )
                            )
                        |> List.sortBy Tuple.first

                otherOption : ( String, Annotation )
                otherOption =
                    ( "Other"
                    , if SeqDict.member characterId characters then
                        ctor (Id.unsafe 0)

                      else
                        ctor characterId
                    )
            in
            select (options ++ [ otherOption ]) annotation
    in
    Html.div
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "4px"
        ]
        [ select selectOptions annotation
        , case annotation of
            Enter _ ->
                characterSelect Enter

            Exit _ ->
                characterSelect Exit

            HappensBefore _ ->
                Html.text ""

            HappensAfter _ ->
                Html.text ""
        , Html.input
            [ Html.Events.onInput changeId
            , Html.Attributes.value ids.idString
            ]
            []
        ]


select : List ( String, value ) -> value -> Html value
select selectOptions currentValue =
    selectOptions
        |> List.map
            (\( label, optionValue ) ->
                Html.option
                    [ Html.Attributes.selected (optionValue == currentValue)
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
                        |> Maybe.withDefault currentValue
                )
            ]
