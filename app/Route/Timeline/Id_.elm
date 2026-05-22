module Route.Timeline.Id_ exposing (..)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Do.Extra as DoExtra
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
import Html.Parser
import Id exposing (Id(..))
import List.Extra
import Maybe.Extra
import OpenApi.Common
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Parser
import Parser.Error
import Result.Extra
import Rope exposing (Rope)
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
    { name : String
    , posts : SeqDict (Id PostDetails) ( PostDetails, List Reply )
    , charactersIcons : SeqDict (Id Character) { icon : Maybe { id : Id Icon, url : Url }, npc : Bool }
    , links : Result ( String, List Parser.DeadEnd ) (List Link)
    }


type Link
    = Symmetric MessageId MessageId
    | Asymmetric { from : MessageId, to : MessageId }


type MessageId
    = MessageIdReply (Id Reply)
    | MessageIdPost (Id PostDetails)


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
    Do.do (GlowficApi.Extra.getBoard authorization continuityId) <| \board ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts authorization continuityId) <| \results ->
    DoExtra.eachCount results (\{ id } -> GlowficApi.Extra.getPost authorization (Id id)) <| \posts ->
    let
        charactersIds : List (Id Character)
        charactersIds =
            List.concatMap (\p -> allCharactersIds p |> SeqDict.keys) posts
                |> SeqSet.fromList
                |> SeqSet.toList
    in
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("Got " ++ String.fromInt (List.length charactersIds) ++ " characters, fetching icons")) <| \() ->
    DoExtra.eachCount charactersIds (\id -> getCharacterIcon authorization id) <| \charactersIcons ->
    { charactersIcons = SeqDict.fromList charactersIcons
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
    , name = board.name
    , links = calculatePostsLinks posts
    }
        |> Response.render
        |> BackendTask.succeed


calculatePostsLinks : List ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (List Link)
calculatePostsLinks posts =
    posts
        |> Result.Extra.combineMap calculatePostLinks
        |> Result.map
            (\rs ->
                rs
                    |> Rope.fromRopeList
                    |> Rope.toList
            )


calculatePostLinks : ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostLinks ( post, replies ) =
    Result.map2 Rope.appendTo
        (calculatePostDetailsLinks post)
        (Result.map Rope.fromRopeList (Result.Extra.combineMap calculateReplyLinks replies))


calculatePostDetailsLinks : PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostDetailsLinks { id, content } =
    calculateContentLinks (MessageIdPost (Id.fromInt id)) content


calculateReplyLinks : Reply -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateReplyLinks { id, content } =
    calculateContentLinks (MessageIdReply (Id.fromInt id)) content


calculateContentLinks : MessageId -> String -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateContentLinks from content =
    case Html.Parser.run Html.Parser.noCharRefs content of
        Err e ->
            Err ( content, e )

        Ok parsed ->
            Ok Rope.empty


errorToHtml :
    String
    -> List Parser.DeadEnd
    -> Html msg
errorToHtml src deadEnds =
    let
        color : String -> Html msg -> Html msg
        color value child =
            Html.span [ Html.Attributes.style "color" value ] [ child ]
    in
    Parser.Error.renderError
        { text = Html.text
        , formatContext = color "cyan"
        , formatCaret = color "red"
        , newline = Html.br [] []
        , linesOfExtraContext = 3
        }
        Parser.Error.forParser
        src
        deadEnds
        |> Html.pre
            [ Html.Attributes.style "overflow" "scroll"
            , Html.Attributes.style "max-width" "calc(100vw-16px)"
            ]


getCharacterIcon :
    { token : String }
    -> Id Character
    ->
        BackendTask
            FatalError
            ( Id Character
            , { icon : Maybe { id : Id Icon, url : Url }
              , npc : Bool
              }
            )
getCharacterIcon authorization id =
    GlowficApi.Extra.getCharacter authorization id
        |> BackendTask.map
            (\character ->
                ( id
                , { icon =
                        case character.default_icon of
                            OpenApi.Common.Null ->
                                Nothing

                            OpenApi.Common.Present icon ->
                                Just { id = Id icon.id, url = icon.url }
                  , npc = character.npc
                  }
                )
            )


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
    { title = app.data.name
    , body =
        app.data.posts
            |> SeqDict.toList
            |> List.map
                (\( _, ( post, replies ) ) ->
                    viewPostSummary app.data post replies
                )
            |> (::) (viewLinks app.data.links)
            |> Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "flex-wrap" "wrap"
                , Html.Attributes.style "align-items" "start"
                , Html.Attributes.style "gap" "8px"
                , Html.Attributes.style "padding" "8px"
                , Html.Attributes.style "color" "#f3f3f3"
                , Html.Attributes.style "background" "#211e2f"
                ]
            |> List.singleton
    }


viewLinks : Result ( String, List Parser.DeadEnd ) (List Link) -> Html msg
viewLinks linksResult =
    case linksResult of
        Err ( content, deadEnds ) ->
            errorToHtml content deadEnds

        Ok links ->
            links
                |> List.map (\link -> Html.li [] [ Html.text (Debug.toString link) ])
                |> Html.ul []


viewPostSummary : Data -> PostDetails -> List Reply -> Html msg
viewPostSummary appData post replies =
    let
        charactersIds : SeqDict (Id Character) (SeqSet String)
        charactersIds =
            allCharactersIds ( post, replies )
    in
    [ Html.a
        [ Html.Attributes.href ("https://glowfic.com/posts/" ++ String.fromInt post.id)
        ]
        [ Html.text post.subject ]
    , charactersIds
        |> SeqDict.toList
        |> List.Extra.stableSortWith
            (\l r ->
                case
                    ( SeqDict.get (Tuple.first l) appData.charactersIcons |> Maybe.andThen .icon
                    , SeqDict.get (Tuple.first r) appData.charactersIcons |> Maybe.andThen .icon
                    )
                of
                    ( Nothing, Just _ ) ->
                        GT

                    ( Just _, Nothing ) ->
                        LT

                    ( Nothing, Nothing ) ->
                        EQ

                    ( Just _, Just _ ) ->
                        EQ
            )
        |> List.filterMap
            (\( characterId, characterNames ) ->
                let
                    characterName : String
                    characterName =
                        characterNames
                            |> SeqSet.toList
                            |> List.map String.trim
                            |> List.Extra.removeWhen String.isEmpty
                            |> String.join ", "

                    textStyle () =
                        Html.div
                            [ Html.Attributes.style "height" "60px"
                            , Html.Attributes.style "width" "120px"
                            , Html.Attributes.style "border" "1px solid white"
                            , Html.Attributes.style "padding" "4px"
                            , Html.Attributes.style "flex" "0 1 120px"
                            ]
                            [ Html.text characterName ]
                in
                case SeqDict.get characterId appData.charactersIcons of
                    Just { icon, npc } ->
                        if npc then
                            Nothing

                        else
                            case icon of
                                Just { id, url } ->
                                    Html.a
                                        [ Html.Attributes.class "icon"
                                        , Html.Attributes.href ("https://glowfic.com/characters/" ++ String.fromInt (Id.toInt characterId))
                                        , Html.Attributes.title characterName
                                        ]
                                        [ Html.img
                                            [ Html.Attributes.style "width" "auto"
                                            , Html.Attributes.style "height" "60px"
                                            , Html.Attributes.src (Url.toString url)
                                            ]
                                            []
                                        ]
                                        |> Just

                                Nothing ->
                                    Just (textStyle ())

                    Nothing ->
                        Just (textStyle ())
            )
        |> Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-wrap" "wrap"
            , Html.Attributes.style "gap" "8px"
            ]
    ]
        |> Html.div
            [ Html.Attributes.style "border" "1px solid white"
            , Html.Attributes.style "padding" "8px"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "class" "thread"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "flex-grow" (String.fromInt (SeqDict.size charactersIds))
            , Html.Attributes.style "max-width" "600px"
            ]
