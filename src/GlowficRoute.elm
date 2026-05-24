module GlowficRoute exposing (character, post, reply)

import Id exposing (CharacterId, Id, PostId, ReplyId)


reply : Id ReplyId -> String
reply rid =
    "https://glowfic.com/replies/" ++ Id.toString rid ++ "#reply-" ++ Id.toString rid


post : Id PostId -> String
post pid =
    "https://glowfic.com/posts/" ++ Id.toString pid


character : Id CharacterId -> String
character cid =
    "https://glowfic.com/characters/" ++ Id.toString cid
