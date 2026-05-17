module Route.Timeline.Id_ exposing (..)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Http as Http
import Dict exposing (Dict)
import Dict.Extra
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Extra
import GlowficApi.Types exposing (Board, Character, Icon, PostDetails, PostSummary, Reply)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Id exposing (Id(..))
import List.Extra
import Maybe.Extra
import Pages.Url
import PagesMsg exposing (PagesMsg)
import RouteBuilder exposing (App, StatelessRoute)
import SeqDict exposing (SeqDict)
import SeqDict.Extra
import SeqSet exposing (SeqSet)
import Server.Response as Response exposing (Response)
import Set
import Url exposing (Url)
import UrlPath
import View exposing (View)
import View.Post


type alias ActionData =
    Never


type alias Data =
    { posts : SeqDict (Id PostDetails) ( PostDetails, List Reply )
    , charactersIcons : SeqDict (Id Character) { id : Id Icon, url : Url }
    }


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { id : String }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRenderWithFallback
        { head = head
        , data = data
        , pages = BackendTask.succeed [ { id = "4968" }, { id = "4902" } ]
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


data : RouteParams -> BackendTask FatalError (Response Data ErrorPage)
data params =
    Do.do
        (case String.toInt params.id of
            Nothing ->
                ("Invalid id: " ++ params.id)
                    |> FatalError.fromString
                    |> BackendTask.fail

            Just i ->
                BackendTask.succeed (Id i)
        )
    <| \continuityId ->
    Do.do GlowficApi.Extra.login <| \authorization ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts authorization continuityId) <| \results ->
    Do.each results (\{ id } -> GlowficApi.Extra.getPost authorization (Id id)) <| \posts ->
    let
        charactersIds : List (Id Character)
        charactersIds =
            List.concatMap (\p -> allCharactersIds p |> SeqDict.keys) posts
                |> SeqSet.fromList
                |> SeqSet.toList
    in
    Do.log ("Got " ++ String.fromInt (List.length charactersIds) ++ " icons") <| \() ->
    Do.each charactersIds (\id -> getCharacterIcon authorization id) <| \charactersIcons ->
    { charactersIcons =
        charactersIcons
            |> List.filterMap (\( f, s ) -> Maybe.map (Tuple.pair f) s)
            |> SeqDict.fromList
    , posts =
        posts
            |> List.map
                (\( p, r ) ->
                    let
                        id : Id PostDetails
                        id =
                            Id p.id
                    in
                    ( id, ( p, r ) )
                )
            |> SeqDict.fromList
    }
        |> Response.render
        |> BackendTask.succeed


getCharacterIcon : { token : String } -> Id Character -> BackendTask FatalError ( Id Character, Maybe { id : Id Icon, url : Url } )
getCharacterIcon authorization id =
    GlowficApi.Extra.getCharacterIcon authorization id
        |> BackendTask.map (\icon -> ( id, icon ))


allCharactersIds : ( PostDetails, List Reply ) -> SeqDict (Id Character) (SeqSet String)
allCharactersIds ( post, replies ) =
    (Maybe.map (\{ id, name } -> { id = id, name = name }) post.character
        :: List.map
            (\{ character, character_name } ->
                Maybe.map
                    (\{ id, name } ->
                        { id = id
                        , name = character_name |> Maybe.withDefault name
                        }
                    )
                    character
            )
            replies
    )
        |> Maybe.Extra.values
        |> List.map (\{ id, name } -> ( Id id, name ))
        |> SeqDict.Extra.groupByWith Tuple.first Tuple.second


view : App Data ActionData RouteParams -> Model -> View msg
view app model =
    { title = "MCU"
    , body =
        app.data.posts
            |> SeqDict.toList
            |> List.map
                (\( _, ( post, replies ) ) ->
                    Html.div
                        [ Html.Attributes.style "border" "1px solid white"
                        , Html.Attributes.style "padding" "8px"
                        , Html.Attributes.style "gap" "8px"
                        , Html.Attributes.style "display" "flex"
                        , Html.Attributes.style "class" "thread"
                        , Html.Attributes.style "flex-direction" "column"
                        , Html.Attributes.style "max-width" "400px"
                        ]
                        (viewPostSummary app.data post replies)
                )
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "gap" "8px"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "color" "#f3f3f3"
                , Html.Attributes.style "background" "#211e2f"
                ]
            |> List.singleton
    }


viewPostSummary : Data -> PostDetails -> List Reply -> List (Html msg)
viewPostSummary appData post replies =
    let
        name =
            post.subject
    in
    [ Html.a
        [ Html.Attributes.href ("https://glowfic.com/posts/" ++ String.fromInt post.id)
        ]
        [ Html.text name ]
    , allCharactersIds ( post, replies )
        |> SeqDict.toList
        |> List.map
            (\( characterId, characterNames ) ->
                let
                    characterName : String
                    characterName =
                        characterNames
                            |> SeqSet.toList
                            |> List.map String.trim
                            |> List.Extra.removeWhen String.isEmpty
                            |> String.join ", "
                in
                case SeqDict.get characterId appData.charactersIcons of
                    Just { id, url } ->
                        Html.a
                            [ Html.Attributes.class "icon"
                            , Html.Attributes.href ("https://glowfic.com/icons/" ++ String.fromInt (Id.toInt id))
                            , Html.Attributes.title characterName
                            ]
                            [ Html.img
                                [ Html.Attributes.style "width" "auto"
                                , Html.Attributes.style "height" "60px"
                                , Html.Attributes.src (Url.toString url)
                                ]
                                []
                            ]

                    Nothing ->
                        Html.div
                            [ Html.Attributes.style "max-width" "90px"
                            , Html.Attributes.style "width" "fit-content"
                            , Html.Attributes.style "height" "60px"
                            , Html.Attributes.style "border" "1px solid white"
                            , Html.Attributes.style "padding" "4px"
                            ]
                            [ Html.text characterName ]
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            ]
    ]
