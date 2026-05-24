module Annotation exposing (Annotation(..), MessageId(..))

import Id exposing (CharacterId, Id, PostId, ReplyId)


type Annotation
    = Enter (Id CharacterId)
    | Exit (Id CharacterId)
    | HappensBefore MessageId
    | HappensAfter MessageId


type MessageId
    = MessageIdReply (Id PostId) (Id ReplyId)
    | MessageIdPost (Id PostId)
