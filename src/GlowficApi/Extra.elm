module GlowficApi.Extra exposing (getPost, login)

import BackendTask exposing (BackendTask)
import BackendTask.Env as Env
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Types exposing (PostDetails, Reply)


login : BackendTask FatalError { token : String }
login =
    BackendTask.map2 Tuple.pair
        (Env.expect "username")
        (Env.expect "password")
        |> BackendTask.allowFatal
        |> BackendTask.andThen
            (\( username, password ) ->
                GlowficApi.Api.login
                    { body =
                        { username = username
                        , password = password
                        }
                    }
                    |> BackendTask.allowFatal
            )


getPost : { token : String } -> Int -> BackendTask FatalError ( PostDetails, List Reply )
getPost { token } id =
    GlowficApi.Api.getPostsId
        { authorization = { authorization = token }
        , params = { id = id }
        }
        |> BackendTask.andThen
            (\post ->
                List.range 0 ((post.num_replies - 1) // 100)
                    |> List.map
                        (\page ->
                            GlowficApi.Api.getPostsIdReplies
                                { authorization = { authorization = token }
                                , params =
                                    { id = id
                                    , page = Just page
                                    , per_page = Just 100
                                    }
                                }
                        )
                    |> BackendTask.combine
                    |> BackendTask.map List.concat
                    |> BackendTask.map (\replies -> ( post, replies ))
            )
        |> BackendTask.allowFatal
