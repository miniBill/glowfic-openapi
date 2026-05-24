module Annotation exposing (Annotation(..), MessageId(..), messageIdToString)

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
