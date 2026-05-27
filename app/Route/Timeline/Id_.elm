module Route.Timeline.Id_ exposing (ActionData, CharacterSummary, Data, Model, Msg, RouteParams, route)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Color.Oklch as Oklch exposing (Oklch)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import Glowfic.Utils
import GlowficApi.Extra
import GlowficApi.Types exposing (PostDetails, Reply, Status(..))
import GlowficRoute
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Id exposing (BoardId, CharacterId, IconId, Id, PostId)
import List.Extra
import Maybe.Extra
import Monad exposing (Monad)
import Monad.Do as Do
import OpenApi.Common
import Pages.Url
import Route
import RouteBuilder exposing (App, StatelessRoute)
import SeqDict exposing (SeqDict)
import SeqDict.Extra
import SeqSet exposing (SeqSet)
import Server.Response as Response exposing (Response)
import String.Extra
import Url exposing (Url)
import UrlPath
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    { name : String
    , posts : SeqDict (Id PostId) { post : ( PostDetails, List Reply ), hasAnnotations : Bool }
    , characters :
        SeqDict
            (Id CharacterId)
            CharacterSummary
    }


type alias CharacterSummary =
    { name : String
    , color : Oklch
    , icon : Maybe { id : Id IconId, url : Url }
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
        , pages =
            BackendTask.succeed
                [--     { id = "4968" }
                 -- , { id = "4902" }
                ]
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
                    boardId : Id BoardId
                    boardId =
                        Id.unsafe i
                in
                Monad.succeed boardId
        )
    <| \continuityId ->
    Do.do (GlowficApi.Extra.getBoard continuityId) <| \board ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts continuityId) <| \results ->
    Do.eachCount results
        (\post ->
            Monad.map2 Tuple.pair
                (GlowficApi.Extra.getPost post.id)
                (Monad.lift (File.exists (Glowfic.Utils.annotationsFilepath post.id)))
        )
    <| \posts ->
    let
        frequency : SeqDict (Id CharacterId) Int
        frequency =
            posts
                |> List.map (\( post, _ ) -> Glowfic.Utils.allCharactersIds post |> SeqDict.map (\_ _ -> 1))
                |> List.foldl
                    (\e a ->
                        SeqDict.merge
                            SeqDict.insert
                            (\k v1 v2 d -> SeqDict.insert k (v1 + v2) d)
                            SeqDict.insert
                            e
                            a
                            SeqDict.empty
                    )
                    SeqDict.empty

        charactersIds : List (Id CharacterId)
        charactersIds =
            frequency
                |> SeqDict.keys
                |> List.sortBy
                    (\id ->
                        SeqDict.get id frequency
                            |> Maybe.withDefault 0
                            |> negate
                    )
    in
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("🧑 Got " ++ String.fromInt (List.length charactersIds) ++ " characters, fetching icons")) <| \() ->
    Do.eachCount (assignColors charactersIds) (\( id, color ) -> getCharacter id color) <| \characters ->
    let
        result : Data
        result =
            { characters =
                characters
                    |> Maybe.Extra.values
                    |> SeqDict.fromList
            , posts =
                posts
                    |> List.map
                        (\( ( p, r ), ann ) ->
                            ( Id.for p, { post = ( p, r ), hasAnnotations = ann } )
                        )
                    |> SeqDict.fromList
            , name = board.name
            }
    in
    result
        |> Response.render
        |> Monad.succeed


assignColors : List id -> List ( id, Oklch )
assignColors list =
    let
        len : Float
        len =
            toFloat (List.length list)
    in
    List.indexedMap (\i e -> ( e, Oklch.oklch 0.7 0.2 (toFloat i / len) )) list


childrenToString : List Html.Parser.Node -> String
childrenToString nodes =
    String.concat (List.map nodeToString nodes)


nodeToString : Html.Parser.Node -> String
nodeToString node =
    case node of
        Html.Parser.Element _ _ children ->
            childrenToString children

        Html.Parser.Text t ->
            t

        Html.Parser.Comment _ ->
            ""


getCharacter :
    Id CharacterId
    -> Oklch
    -> Monad (Maybe ( Id CharacterId, CharacterSummary ))
getCharacter id color =
    GlowficApi.Extra.getCharacter id
        |> Monad.map
            (\character ->
                if character.npc then
                    Nothing

                else
                    Just
                        ( id
                        , { name = character.name
                          , color = color
                          , icon =
                                case character.default_icon of
                                    OpenApi.Common.Null ->
                                        Nothing

                                    OpenApi.Common.Present icon ->
                                        Just { id = icon.id, url = icon.url }
                          }
                        )
            )


view : App Data ActionData RouteParams -> Model -> View msg
view app _ =
    { title = app.data.name
    , body =
        let
            columns : List String
            columns =
                SeqDict.keys app.data.characters
                    |> List.map (\id -> "[c" ++ Id.toString id ++ "-start] auto")

            rows : List String
            rows =
                SeqDict.keys app.data.posts
                    |> List.map (\id -> "[p" ++ Id.toString id ++ "-start] auto")
        in
        [ Html.div
            [ Html.Attributes.style "display" "flex"
            , Html.Attributes.style "color" "#f3f3f3"
            , Html.Attributes.style "background" "#211e2f"
            , Html.Attributes.style "gap" "8px"
            ]
            [ app.data.posts
                |> SeqDict.values
                |> List.concatMap
                    (\{ post } ->
                        viewPostCharacters app.data post
                    )
                |> (++) (viewPostTitles app.data)
                |> (++) (viewCharacterNames app.data)
                |> Html.div
                    [ Html.Attributes.style "display" "grid"
                    , Html.Attributes.style "flex" "1"
                    , Html.Attributes.style "gap" "8px"
                    , Html.Attributes.style "padding" "8px"
                    , Html.Attributes.style "overflow" "scroll"
                    , Html.Attributes.style "max-width" "100vw"
                    , Html.Attributes.style "grid-template-rows"
                        (String.join " " ("[post-name-start] auto" :: columns))
                    , Html.Attributes.style "grid-template-columns"
                        (String.join " " ("[character-name-start] auto" :: rows))
                    ]
            , viewCharacters app.data.characters
            ]
        ]
    }


viewCharacters : SeqDict (Id CharacterId) CharacterSummary -> Html msg
viewCharacters characters =
    characters
        |> SeqDict.toList
        |> List.concatMap viewCharacter
        |> Html.div
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "grid-template-columns" "40px auto"
            , Html.Attributes.style "gap" "8px"
            , Html.Attributes.style "width" "200px"
            ]


viewCharacter : ( Id CharacterId, CharacterSummary ) -> List (Html msg)
viewCharacter ( characterId, { name, icon, color } ) =
    [ case icon of
        Just { url } ->
            Html.a
                [ Html.Attributes.class "icon"
                , Html.Attributes.href (GlowficRoute.character characterId)
                , Html.Attributes.title name
                , Html.Attributes.style "display" "block"
                ]
                [ Html.img
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "auto"
                    , Html.Attributes.src (Url.toString url)
                    ]
                    []
                ]

        Nothing ->
            Html.div [] []
    , Html.div
        [ Html.Attributes.style "background" (Oklch.toCssString color)
        , if color.lightness > 0.5 then
            Html.Attributes.style "color" "black"

          else
            Html.Attributes.style "color" "white"
        ]
        [ Html.text name ]
    ]


viewCharacterNames : Data -> List (Html msg)
viewCharacterNames appData =
    appData.characters
        |> SeqDict.toList
        |> List.map
            (\( id, { name, color } ) ->
                Html.a
                    [ Html.Attributes.href (GlowficRoute.character id)
                    , Html.Attributes.style "display" "block"
                    , Html.Attributes.style "grid-column-start" "character-name-start"
                    , Html.Attributes.style "grid-row-start" ("c" ++ Id.toString id ++ "-start")
                    , Html.Attributes.style "background" (Oklch.toCssString color)
                    , if color.lightness > 0.5 then
                        Html.Attributes.style "color" "black"

                      else
                        Html.Attributes.style "color" "white"
                    ]
                    [ Html.text name ]
            )


viewPostTitles : Data -> List (Html msg)
viewPostTitles appData =
    appData.posts
        |> SeqDict.values
        |> List.map
            (\postData ->
                let
                    ( post, _ ) =
                        postData.post

                    cutTitle : String
                    cutTitle =
                        String.Extra.ellipsis 40 post.subject
                in
                Route.link
                    [ Html.Attributes.style "display" "block"
                    , Html.Attributes.style "grid-row-start" "post-name-start"
                    , Html.Attributes.style "grid-column-start" ("p" ++ Id.toString post.id ++ "-start")
                    , Html.Attributes.style "writing-mode" "vertical-rl"
                    , Html.Attributes.style "text-orientation" "mixed"
                    ]
                    [ [ if postData.hasAnnotations then
                            ""

                        else
                            "⚠️"
                      , case post.status of
                            Status__Complete ->
                                "✅"

                            Status__Active ->
                                "✍️"
                      , cutTitle
                      ]
                        |> List.Extra.removeWhen String.isEmpty
                        |> String.join " "
                        |> Html.text
                    ]
                    (Route.Timeline__Post__Id_ { id = Id.toString post.id })
            )



-- viewLinksAsGraph : Data -> Html msg
-- viewLinksAsGraph appData =
--     case appData.links of
--         Err ( content, deadEnds ) ->
--             errorToHtml content deadEnds
--         Ok links ->
--             let
--                 toPostId : MessageId -> Id PostId
--                 toPostId messageId =
--                     case messageId of
--                         MessageIdPost pid ->
--                             pid
--                         MessageIdReply pid _ ->
--                             pid
--                 posts : SeqSet (Id PostId)
--                 posts =
--                     links
--                         |> List.concatMap
--                             (\link ->
--                                 [ toPostId link.from
--                                 , toPostId link.to
--                                 ]
--                             )
--                         |> SeqSet.fromList
--                 nodes : Result (Html msg) String
--                 nodes =
--                     posts
--                         |> SeqSet.toList
--                         |> Result.Extra.combineMap
--                             (\pid ->
--                                 let
--                                     pidString =
--                                         Id.toString pid
--                                 in
--                                 case SeqDict.get pid appData.posts of
--                                     Nothing ->
--                                         "Could not find post {pid}"
--                                             |> String.replace "{pid}" pidString
--                                             |> Err
--                                     Just ( post, _ ) ->
--                                         (pidString ++ " " ++ post.subject)
--                                             |> Ok
--                             )
--                         |> Result.map (String.join "\n")
--                         |> Result.mapError Html.text
--                 edges : Result (Html msg) String
--                 edges =
--                     case appData.links of
--                         Err ( s, e ) ->
--                             Err (errorToHtml s e)
--                         Ok ls ->
--                             ls
--                                 |> List.map
--                                     (\{ from, to, label } ->
--                                         [ Id.toString (toPostId from)
--                                         , Id.toString (toPostId to)
--                                         , label
--                                         ]
--                                             |> String.join " "
--                                     )
--                                 |> String.join "\n"
--                                 |> Ok
--             in
--             Html.pre []
--                 [ case Result.map2 Tuple.pair nodes edges of
--                     Ok ( n, e ) ->
--                         Html.text (n ++ "\n#\n" ++ e)
--                     Err e ->
--                         e
--                 ]
-- viewLinkEndpoint : MessageId -> Html msg
-- viewLinkEndpoint id =
--     case id of
--         MessageIdPost pid ->
--             Html.a
--                 [ Html.Attributes.href (GlowficRoute.post pid)
--                 ]
--                 [ "Post {pid}"
--                     |> String.replace "{pid}" (Id.toString pid)
--                     |> Html.text
--                 ]
--         MessageIdReply pid rid ->
--             Html.a
--                 [ Html.Attributes.href (GlowficRoute.reply rid)
--                 ]
--                 [ "Reply {rid} from post {pid}"
--                     |> String.replace "{rid}" (Id.toString rid)
--                     |> String.replace "{pid}" (Id.toString pid)
--                     |> Html.text
--                 ]


viewPostCharacters : Data -> ( PostDetails, List Reply ) -> List (Html msg)
viewPostCharacters appData ( post, replies ) =
    let
        charactersIds : SeqDict (Id CharacterId) (SeqSet String)
        charactersIds =
            Glowfic.Utils.allCharactersIds ( post, replies )
    in
    charactersIds
        |> SeqDict.keys
        |> List.Extra.stableSortWith
            (\l r ->
                case
                    ( SeqDict.get l appData.characters |> Maybe.andThen .icon
                    , SeqDict.get r appData.characters |> Maybe.andThen .icon
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
        |> List.map
            (\characterId ->
                Html.div
                    [ -- Html.Attributes.style "height" "60px"
                      -- , Html.Attributes.style "width" "10px"
                      -- ,
                      Html.Attributes.style "border" "1px solid white"
                    , Html.Attributes.style "padding" "4px"
                    , Html.Attributes.style "grid-row-start" ("c" ++ Id.toString characterId ++ "-start")
                    , Html.Attributes.style "grid-column-start" ("p" ++ Id.toString post.id ++ "-start")
                    ]
                    [ Html.text "X" ]
            )
