module Route.Timeline.Id_ exposing (ActionData, Data, Link, MessageId, Model, Msg, RouteParams, route)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Do.Extra as DoExtra
import Color.Oklch as Oklch exposing (Oklch)
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (Board, Character, Icon, PostDetails, Reply)
import GlowficRoute
import Head
import Head.Seo as Seo
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Parser
import Id exposing (BoardId, CharacterId, IconId, Id, PostId, ReplyId)
import List.Extra
import Maybe.Extra
import OpenApi.Common
import Pages.Url
import Parser exposing ((|.), (|=), Parser)
import Result.Extra
import Rope exposing (Rope)
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
    , posts : SeqDict (Id PostId) ( PostDetails, List Reply )
    , characters :
        SeqDict
            (Id CharacterId)
            CharacterSummary
    , links : Result ( String, List Parser.DeadEnd ) (List Link)
    }


type alias CharacterSummary =
    { name : String
    , color : Oklch
    , icon : Maybe { id : Id IconId, url : Url }
    }


type alias Link =
    { from : MessageId
    , to : MessageId
    , label : String
    }


type MessageId
    = MessageIdReply (Id PostId) (Id ReplyId)
    | MessageIdPost (Id PostId)


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
    Do.do
        (case String.toInt params.id of
            Nothing ->
                ("Invalid id: " ++ params.id)
                    |> FatalError.fromString
                    |> BackendTask.fail

            Just i ->
                let
                    boardId : Id BoardId
                    boardId =
                        Id.unsafe i
                in
                BackendTask.succeed boardId
        )
    <| \continuityId ->
    Do.do GlowficApi.Extra.login <| \( authorization, break1 ) ->
    Do.do (GlowficApi.Extra.getBoard break1 authorization continuityId) <| \( board, break2 ) ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts break2 authorization continuityId) <| \( results, break3 ) ->
    DoExtra.eachCountWithCircuitBreaker break3 results (\brk post -> GlowficApi.Extra.getPost brk authorization post.id) <| \( posts, break4 ) ->
    let
        frequency : SeqDict (Id CharacterId) Int
        frequency =
            posts
                |> List.map (\post -> allCharactersIds post |> SeqDict.map (\_ _ -> 1))
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
    DoExtra.eachCountWithCircuitBreaker break4 (assignColors charactersIds) (\brk ( id, color ) -> getCharacter brk authorization id color) <| \( characters, _ ) ->
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

        result : Data
        result =
            { characters =
                characters
                    |> Maybe.Extra.values
                    |> SeqDict.fromList
            , posts =
                posts
                    |> List.map
                        (\( p, r ) ->
                            ( Id.for p, ( p, r ) )
                        )
                    |> SeqDict.fromList
            , name = board.name
            , links = calculatePostsLinks replyToPost posts
            }
    in
    result
        |> Response.render
        |> BackendTask.succeed


assignColors : List id -> List ( id, Oklch )
assignColors list =
    let
        len : Float
        len =
            toFloat (List.length list)
    in
    List.indexedMap (\i e -> ( e, Oklch.oklch 0.7 0.2 (toFloat i / len) )) list


calculatePostsLinks : SeqDict (Id ReplyId) (Id PostId) -> List ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (List Link)
calculatePostsLinks replyToPost posts =
    if 3 > 0 then
        Ok []

    else
        posts
            |> Result.Extra.combineMap (calculatePostLinks replyToPost)
            |> Result.map
                (\rs ->
                    rs
                        |> Rope.fromRopeList
                        |> Rope.toList
                )


calculatePostLinks : SeqDict (Id ReplyId) (Id PostId) -> ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostLinks replyToPost ( post, replies ) =
    Result.map2 Rope.appendTo
        (calculatePostDetailsLinks replyToPost post)
        (Result.map Rope.fromRopeList (Result.Extra.combineMap (calculateReplyLinks replyToPost post) replies))


calculatePostDetailsLinks : SeqDict (Id ReplyId) (Id PostId) -> PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostDetailsLinks replyToPost ({ content } as p) =
    calculateContentLinks replyToPost (MessageIdPost (Id.for p)) content


calculateReplyLinks : SeqDict (Id ReplyId) (Id PostId) -> PostDetails -> Reply -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateReplyLinks replyToPost post reply =
    calculateContentLinks replyToPost (MessageIdReply (Id.for post) (Id.for reply)) reply.content


calculateContentLinks : SeqDict (Id ReplyId) (Id PostId) -> MessageId -> String -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateContentLinks replyToPost from content =
    case Html.Parser.run Html.Parser.noCharRefs content of
        Err e ->
            Err ( content, e )

        Ok parsed ->
            parsed
                |> Result.Extra.combineMap (calculateNodeLinks replyToPost from)
                |> Result.map Rope.fromRopeList


calculateNodeLinks : SeqDict (Id ReplyId) (Id PostId) -> MessageId -> Html.Parser.Node -> Result ( String, List Parser.DeadEnd ) (Rope Link)
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
                            Ok (Rope.singleton { from = from, to = to, label = childrenToString children })

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
                                ("While parsing " ++ messageIdToString from ++ ", could not find post for reply id " ++ Id.toString id)
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


getCharacter :
    { got429 : Bool }
    -> { token : String }
    -> Id CharacterId
    -> Oklch
    ->
        BackendTask
            FatalError
            ( Maybe ( Id CharacterId, CharacterSummary )
            , { got429 : Bool }
            )
getCharacter got429 authorization id color =
    GlowficApi.Extra.getCharacter got429 authorization id
        |> BackendTask.map
            (\( character, newGot429 ) ->
                ( if character.npc then
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
                , newGot429
                )
            )


allCharactersIds : ( PostDetails, List Reply ) -> SeqDict (Id CharacterId) (SeqSet String)
allCharactersIds ( post, replies ) =
    let
        replyToCharacter : Reply -> Maybe ( Id CharacterId, String )
        replyToCharacter reply =
            Maybe.map
                (\character ->
                    ( Id.for character
                    , reply.character_name |> Maybe.withDefault character.name
                    )
                )
                reply.character
    in
    (Maybe.map (\character -> ( Id.for character, character.name )) post.character
        :: List.map replyToCharacter replies
    )
        |> Maybe.Extra.values
        |> SeqDict.Extra.groupByWith Tuple.first Tuple.second


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
                    (\( post, replies ) ->
                        viewPostCharacters app.data post replies
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
            (\( post, _ ) ->
                Html.a
                    [ Html.Attributes.href (GlowficRoute.post (Id.for post))
                    , Html.Attributes.style "display" "block"
                    , Html.Attributes.style "grid-row-start" "post-name-start"
                    , Html.Attributes.style "grid-column-start" ("p" ++ Id.toString post.id ++ "-start")
                    , Html.Attributes.style "writing-mode" "vertical-rl"
                    , Html.Attributes.style "text-orientation" "mixed"
                    ]
                    [ Html.text (String.Extra.ellipsis (String.length "Eighty-Eight Million Eight Hundred and Eighty-Eight Tho") post.subject)
                    ]
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


messageIdToString : MessageId -> String
messageIdToString id =
    case id of
        MessageIdPost pid ->
            "Post {pid}"
                |> String.replace "{pid}" (Id.toString pid)

        MessageIdReply pid rid ->
            "Reply {rid} from post {pid}"
                |> String.replace "{rid}" (Id.toString rid)
                |> String.replace "{pid}" (Id.toString pid)


viewPostCharacters : Data -> PostDetails -> List Reply -> List (Html msg)
viewPostCharacters appData post replies =
    let
        charactersIds : SeqDict (Id CharacterId) (SeqSet String)
        charactersIds =
            allCharactersIds ( post, replies )
    in
    charactersIds
        |> SeqDict.toList
        |> List.Extra.stableSortWith
            (\l r ->
                case
                    ( SeqDict.get (Tuple.first l) appData.characters |> Maybe.andThen .icon
                    , SeqDict.get (Tuple.first r) appData.characters |> Maybe.andThen .icon
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
            (\( characterId, characterNames ) ->
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
