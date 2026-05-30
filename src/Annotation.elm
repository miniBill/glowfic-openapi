module Annotation exposing (Annotation(..), MessageId(..), codec, messageIdCodec)

import Codec exposing (Codec)
import Id exposing (CharacterId, Id, PostId, ReplyId)


type Annotation
    = Enter (Id CharacterId)
    | Exit (Id CharacterId)
    | HappensBefore MessageId
    | HappensAfter MessageId


type MessageId
    = MessageIdReply (Id ReplyId)
    | MessageIdPost (Id PostId)


messageIdToString : MessageId -> String
messageIdToString id =
    case id of
        MessageIdPost pid ->
            "Post {pid}"
                |> String.replace "{pid}" (Id.toString pid)

        MessageIdReply rid ->
            "Reply {rid}"
                |> String.replace "{rid}" (Id.toString rid)


codec : Codec Annotation
codec =
    Codec.custom
        (\fEnter fExit fHappensBefore fHappensAfter value ->
            case value of
                Enter id ->
                    fEnter id

                Exit id ->
                    fExit id

                HappensBefore id ->
                    fHappensBefore id

                HappensAfter id ->
                    fHappensAfter id
        )
        |> Codec.variant1 "Enter" Enter Id.codec
        |> Codec.variant1 "Exit" Exit Id.codec
        |> Codec.variant1 "HappensBefore" HappensBefore messageIdCodec
        |> Codec.variant1 "HappensAfter" HappensAfter messageIdCodec
        |> Codec.buildCustom


messageIdCodec : Codec MessageId
messageIdCodec =
    Codec.custom
        (\fMessageIdReply fMessageIdPost value ->
            case value of
                MessageIdReply id ->
                    fMessageIdReply id

                MessageIdPost id ->
                    fMessageIdPost id
        )
        |> Codec.variant1 "MessageIdReply" MessageIdReply Id.codec
        |> Codec.variant1 "MessageIdPost" MessageIdPost Id.codec
        |> Codec.buildCustom
