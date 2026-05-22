module GlowficRoute exposing (character, post, reply)

import GlowficApi.Types exposing (Character, PostDetails, Reply)
import Id exposing (Id)


reply : Id Reply -> String
reply rid =
    "https://glowfic.com/replies/" ++ Id.toString rid ++ "#reply-" ++ Id.toString rid


post : Id PostDetails -> String
post pid =
    "https://glowfic.com/posts/" ++ Id.toString pid


character : Id Character -> String
character cid =
    "https://glowfic.com/characters/" ++ Id.toString cid
