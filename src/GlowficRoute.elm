module GlowficRoute exposing (character, icon, post, reply)

import Id exposing (CharacterId, IconId, Id, PostId, ReplyId)


reply : Id ReplyId -> String
reply rid =
    "https://glowfic.com/replies/" ++ Id.toString rid ++ "#reply-" ++ Id.toString rid


post : Id PostId -> String
post pid =
    "https://glowfic.com/posts/" ++ Id.toString pid


character : Id CharacterId -> String
character cid =
    "https://glowfic.com/characters/" ++ Id.toString cid


icon : Id IconId -> String
icon cid =
    "https://glowfic.com/icons/" ++ Id.toString cid
