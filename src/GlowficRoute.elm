module GlowficRoute exposing (character, post, reply)

import GlowficApi.Types exposing (Character, PostDetails, Reply)
import Id exposing (Id)


reply : Id Reply -> String
reply rid =
    "https://glowfic.com/replies/" ++ String.fromInt (Id.toInt rid) ++ "#reply-" ++ String.fromInt (Id.toInt rid)


post : Id PostDetails -> String
post pid =
    "https://glowfic.com/posts/" ++ String.fromInt (Id.toInt pid)


character : Id Character -> String
character cid =
    "https://glowfic.com/characters/" ++ String.fromInt (Id.toInt cid)
