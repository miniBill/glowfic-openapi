module GlowficRoute exposing (character, post, reply)

import GlowficApi.Types exposing (Character, PostDetails, Reply)
import Id exposing (Id(..))


reply : Id Reply -> String
reply (Id rid) =
    "https://glowfic.com/replies/" ++ String.fromInt rid ++ "#reply-" ++ String.fromInt rid


post : Id PostDetails -> String
post (Id pid) =
    "https://glowfic.com/posts/" ++ String.fromInt pid


character : Id Character -> String
character (Id cid) =
    "https://glowfic.com/characters/" ++ String.fromInt cid
