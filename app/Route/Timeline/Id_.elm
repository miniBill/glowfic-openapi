module Route.Timeline.Id_ exposing (ActionData, Data, Link, MessageId, Model, Msg, RouteParams, route)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Do.Extra as DoExtra
import ErrorPage exposing (ErrorPage)
import FatalError exposing (FatalError)
import GlowficApi.Extra
import GlowficApi.Types exposing (Board, Character, Icon, PostDetails, Reply)
import GlowficRoute
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes
import Html.Parser
import Id exposing (Id)
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
import Url exposing (Url)
import UrlPath
import View exposing (View)


type alias ActionData =
    Never


type alias Data =
    { name : String
    , posts : SeqDict (Id PostDetails) ( PostDetails, List Reply )
    , charactersIcons : SeqDict (Id Character) { icon : Maybe { id : Id Icon, url : Url }, npc : Bool }
    , links : Result ( String, List Parser.DeadEnd ) (List Link)
    }


type alias Link =
    { from : MessageId
    , to : MessageId
    , label : String
    }


type MessageId
    = MessageIdReply (Id PostDetails) (Id Reply)
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
                let
                    boardId : Id Board
                    boardId =
                        Id.unsafe i
                in
                BackendTask.succeed boardId
        )
    <| \continuityId ->
    Do.do GlowficApi.Extra.login <| \authorization ->
    Do.do (GlowficApi.Extra.getBoard authorization continuityId) <| \board ->
    Do.do (GlowficApi.Extra.getAllBoardsIdPosts authorization continuityId) <| \results ->
    DoExtra.eachCount results (\post -> GlowficApi.Extra.getPost authorization (Id.unsafe post.id)) <| \posts ->
    let
        charactersIds : List (Id Character)
        charactersIds =
            List.concatMap (\p -> allCharactersIds p |> SeqDict.keys) posts
                |> SeqSet.fromList
                |> SeqSet.toList
    in
    Do.log (Ansi.Color.fontColor Ansi.Color.cyan ("🧑 Got " ++ String.fromInt (List.length charactersIds) ++ " characters, fetching icons")) <| \() ->
    DoExtra.eachCount charactersIds (\id -> getCharacterIcon authorization id) <| \charactersIcons ->
    let
        replyToPost : SeqDict (Id Reply) (Id PostDetails)
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
            { charactersIcons = SeqDict.fromList charactersIcons
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


calculatePostsLinks : SeqDict (Id Reply) (Id PostDetails) -> List ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (List Link)
calculatePostsLinks replyToPost posts =
    posts
        |> Result.Extra.combineMap (calculatePostLinks replyToPost)
        |> Result.map
            (\rs ->
                rs
                    |> Rope.fromRopeList
                    |> Rope.toList
            )


calculatePostLinks : SeqDict (Id Reply) (Id PostDetails) -> ( PostDetails, List Reply ) -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostLinks replyToPost ( post, replies ) =
    Result.map2 Rope.appendTo
        (calculatePostDetailsLinks replyToPost post)
        (Result.map Rope.fromRopeList (Result.Extra.combineMap (calculateReplyLinks replyToPost post) replies))


calculatePostDetailsLinks : SeqDict (Id Reply) (Id PostDetails) -> PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculatePostDetailsLinks replyToPost ({ content } as p) =
    calculateContentLinks replyToPost (MessageIdPost (Id.for p)) content


calculateReplyLinks : SeqDict (Id Reply) (Id PostDetails) -> PostDetails -> Reply -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateReplyLinks replyToPost post reply =
    calculateContentLinks replyToPost (MessageIdReply (Id.for post) (Id.for reply)) reply.content


calculateContentLinks : SeqDict (Id Reply) (Id PostDetails) -> MessageId -> String -> Result ( String, List Parser.DeadEnd ) (Rope Link)
calculateContentLinks replyToPost from content =
    case Html.Parser.run Html.Parser.noCharRefs content of
        Err e ->
            Err ( content, e )

        Ok parsed ->
            parsed
                |> Result.Extra.combineMap (calculateNodeLinks replyToPost from)
                |> Result.map Rope.fromRopeList


calculateNodeLinks : SeqDict (Id Reply) (Id PostDetails) -> MessageId -> Html.Parser.Node -> Result ( String, List Parser.DeadEnd ) (Rope Link)
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


targetParser : SeqDict (Id Reply) (Id PostDetails) -> MessageId -> Parser (Maybe MessageId)
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
                                Just { id = Id.unsafe icon.id, url = icon.url }
                  , npc = character.npc
                  }
                )
            )


allCharactersIds : ( PostDetails, List Reply ) -> SeqDict (Id Character) (SeqSet String)
allCharactersIds ( post, replies ) =
    let
        replyToCharacter : Reply -> Maybe ( Id Character, String )
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
        app.data.posts
            |> SeqDict.toList
            |> List.map
                (\( _, ( post, replies ) ) ->
                    viewPostSummary app.data post replies
                )
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



-- viewLinksAsGraph : Data -> Html msg
-- viewLinksAsGraph appData =
--     case appData.links of
--         Err ( content, deadEnds ) ->
--             errorToHtml content deadEnds
--         Ok links ->
--             let
--                 toPostId : MessageId -> Id PostDetails
--                 toPostId messageId =
--                     case messageId of
--                         MessageIdPost pid ->
--                             pid
--                         MessageIdReply pid _ ->
--                             pid
--                 posts : SeqSet (Id PostDetails)
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


viewPostSummary : Data -> PostDetails -> List Reply -> Html msg
viewPostSummary appData post replies =
    let
        charactersIds : SeqDict (Id Character) (SeqSet String)
        charactersIds =
            allCharactersIds ( post, replies )
    in
    [ Html.a
        [ Html.Attributes.href (GlowficRoute.post (Id.for post))
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
                                Just { url } ->
                                    Html.a
                                        [ Html.Attributes.class "icon"
                                        , Html.Attributes.href (GlowficRoute.character characterId)
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
