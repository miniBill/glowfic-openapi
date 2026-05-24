module Annotation exposing (Annotation(..), MessageId(..), messageIdToString)

import Id exposing (CharacterId, Id, PostId, ReplyId)


type Annotation
    = Enter (Id CharacterId)
    | Exit (Id CharacterId)
    | HappensBefore MessageId
    | HappensAfter MessageId


type MessageId
    = MessageIdReply (Id PostId) (Id ReplyId)
    | MessageIdPost (Id PostId)


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
