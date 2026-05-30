module Glowfic.Utils exposing (allCharactersIds, annotationsCodec, boardAnnotationsFilename, boardAnnotationsFilepath, calculatePostAnnotations, postAnnotationsFilename, readAnnotationsFromFile)

import Annotation exposing (Annotation(..), MessageId(..))
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Codec exposing (Codec)
import FatalError exposing (FatalError)
import GlowficApi.Types exposing (PostDetails, Reply)
import Html.Parser
import Id exposing (BoardId, CharacterId, Id, PostId)
import Json.Decode
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Extra
import Result.Extra
import Rope exposing (Rope)
import SeqDict exposing (SeqDict)
import SeqDict.Extra
import SeqSet exposing (SeqSet)


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


postAnnotationsFilepath : Id PostId -> String
postAnnotationsFilepath postId =
    "data/" ++ postAnnotationsFilename postId


boardAnnotationsFilepath : Id BoardId -> String
boardAnnotationsFilepath boardId =
    "data/" ++ boardAnnotationsFilename boardId


postAnnotationsFilename : Id PostId -> String
postAnnotationsFilename postId =
    "annotations-" ++ Id.toString postId ++ ".json"


boardAnnotationsFilename : Id BoardId -> String
boardAnnotationsFilename boardId =
    "annotations-board-" ++ Id.toString boardId ++ ".json"


readAnnotationsFromFile : Id PostId -> BackendTask FatalError (Maybe (List ( MessageId, Annotation )))
readAnnotationsFromFile postId =
    File.rawFile (postAnnotationsFilepath postId)
        |> BackendTask.toResult
        |> BackendTask.andThen
            (\raw ->
                case raw of
                    Err e ->
                        case e.recoverable of
                            File.FileDoesntExist ->
                                BackendTask.succeed Nothing

                            File.FileReadError _ ->
                                BackendTask.fail e.fatal

                            File.DecodingError _ ->
                                BackendTask.fail e.fatal

                    Ok rawString ->
                        case Codec.decodeString annotationsCodec rawString of
                            Ok v ->
                                v
                                    |> SeqDict.toList
                                    |> List.concatMap
                                        (\( messageId, annotations ) ->
                                            List.map (Tuple.pair messageId) annotations
                                        )
                                    |> Just
                                    |> BackendTask.succeed

                            Err e ->
                                BackendTask.fail (FatalError.fromString (Json.Decode.errorToString e))
            )


annotationsCodec : Codec (SeqDict MessageId (List Annotation))
annotationsCodec =
    Codec.tuple Annotation.messageIdCodec (Codec.list Annotation.codec)
        |> Codec.list
        |> Codec.map SeqDict.fromList SeqDict.toList


calculatePostAnnotations : ( PostDetails, List Reply ) -> Result FatalError (List ( MessageId, Annotation ))
calculatePostAnnotations ( post, replies ) =
    Result.map2
        (\l r -> Rope.appendTo l r |> Rope.toList)
        (calculatePostDetailsAnnotations post)
        (Result.map Rope.fromRopeList (Result.Extra.combineMap calculateReplyAnnotations replies))
        |> Result.mapError (\( t, e ) -> FatalError.fromString (Parser.Extra.errorToString t e))


calculatePostDetailsAnnotations : PostDetails -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculatePostDetailsAnnotations ({ content } as p) =
    calculateContentAnnotations (MessageIdPost (Id.for p)) content


calculateReplyAnnotations : Reply -> Result ( String, List Parser.DeadEnd ) (Rope ( MessageId, Annotation ))
calculateReplyAnnotations reply =
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
                    case Parser.run targetParser target of
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


targetParser : Parser (Maybe MessageId)
targetParser =
    Parser.oneOf
        [ Parser.succeed (\reply -> Just (MessageIdReply (Id.unsafe reply)))
            |. Parser.oneOf
                [ Parser.token "/replies/"
                , Parser.token "https://glowfic.com/replies/"
                , Parser.token "https://www.glowfic.com/replies/"
                ]
            |= Parser.int
            |. Parser.token "#reply-"
            |. Parser.int
        , Parser.succeed (\post -> Just (MessageIdPost (Id.unsafe post)))
            |. Parser.oneOf
                [ Parser.token "/posts/"
                , Parser.token "https://glowfic.com/posts/"
                , Parser.token "https://www.glowfic.com/posts/"
                ]
            |= Parser.int
        , Parser.succeed Nothing
            |. Parser.oneOf
                [ Parser.token "https://en.wikipedia.org/"
                , Parser.token "https://www.aonprd.com/"
                , Parser.token "https://www.d20pfsrd.com/"
                , Parser.token "http://www.chinese-poems.com/"
                , Parser.token "https://www.willowandroxas.com/"
                ]
            |. Parser.chompWhile (\_ -> True)
        ]
        |. Parser.end
