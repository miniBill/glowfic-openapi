module GlowficApi.Extra exposing (getAllBoardsIdPosts, getBoard, getCharacter, getPost)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.Env as Env
import BackendTask.File as File
import BackendTask.Http as Http exposing (Body, Expect)
import BackendTask.Time as Time
import Dict
import Duration exposing (Duration)
import FatalError exposing (FatalError)
import GlowficApi.Api
import GlowficApi.Types exposing (Board, CharacterDetails, PostDetails, PostSummary, Reply)
import Id exposing (BoardId, CharacterId, Id, PostId, SectionId)
import List.Extra
import Monad exposing (Monad)
import Pages.Script as Script
import Quantity
import Time as CoreTime


getCharacter :
    Id CharacterId
    -> Monad CharacterDetails
getCharacter id =
    Monad.getRefreshingIf (\_ -> False)
        ("getCharacter " ++ Id.toString id)
        GlowficApi.Api.getCharactersIdRecord
        { params =
            { id = id
            , post_id = Nothing
            }
        }


getPost : Id PostId -> Monad ( PostDetails, List Reply )
getPost id =
    Monad.getRefreshingIf (\post -> post.status /= GlowficApi.Types.Status__Complete)
        ("getPost " ++ Id.toString id)
        GlowficApi.Api.getPostsIdRecord
        { params = { id = id }
        }
        |> Monad.andThen
            (\post ->
                let
                    maxPage : Int
                    maxPage =
                        ((post.num_replies - 1) // 100) + 1
                in
                List.range 1 maxPage
                    |> List.map
                        (\page ->
                            Monad.getRefreshingIf (\_ -> page == maxPage && post.status /= GlowficApi.Types.Status__Complete)
                                ("getPost " ++ Id.toString id ++ " page " ++ String.fromInt page)
                                GlowficApi.Api.getPostsIdRepliesRecord
                                { params =
                                    { id = id
                                    , page = Just page
                                    , per_page = Just 100
                                    }
                                }
                        )
                    |> Monad.combine
                    |> Monad.map (\replies -> ( post, List.concat replies ))
            )


getBoard :
    Id BoardId
    ->
        Monad
            { id : Id BoardId
            , name : String
            , board_sections : List { id : Id SectionId, name : String, order : Int }
            }
getBoard id =
    Monad.getRefreshingIf (\_ -> True)
        ("getBoard " ++ Id.toString id)
        GlowficApi.Api.getBoardsIdRecord
        { params = { id = id } }


getAllBoardsIdPosts :
    Id BoardId
    -> Monad (List PostSummary)
getAllBoardsIdPosts continuityId =
    let
        go :
            Int
            -> List (List PostSummary)
            -> Monad (List PostSummary)
        go page acc =
            Monad.getRefreshingIf (\_ -> True)
                ("getAllBoardsIdPosts " ++ Id.toString continuityId)
                GlowficApi.Api.getBoardsIdPostsRecord
                { params =
                    { id = continuityId
                    , page = Just page
                    }
                }
                |> Monad.andThen
                    (\{ results } ->
                        if List.isEmpty results then
                            (acc
                                |> List.reverse
                                |> List.concat
                            )
                                |> Monad.succeed

                        else
                            go (page + 1) (results :: acc)
                    )
    in
    go 1 []
