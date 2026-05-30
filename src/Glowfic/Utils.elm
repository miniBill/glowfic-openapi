module Glowfic.Utils exposing (allCharactersIds, boardAnnotationsFilepath, postAnnotationsFilename, postAnnotationsFilepath)

import GlowficApi.Types exposing (PostDetails, Reply)
import Id exposing (BoardId, CharacterId, Id, PostId)
import Maybe.Extra
import SeqDict exposing (SeqDict)
import SeqDict.Extra
import SeqSet exposing (SeqSet)


allCharactersIds : ( PostDetails, List Reply ) -> SeqDict (Id CharacterId) (SeqSet String)
allCharactersIds ( post, replies ) =
    let
        replyToCharacter : Reply -> Maybe ( Id CharacterId, String )
        replyToCharacter reply =
            Maybe.map
                (\character ->
                    ( Id.for character
                    , reply.character_name |> Maybe.withDefault character.name
                    )
                )
                reply.character
    in
    (Maybe.map (\character -> ( Id.for character, character.name )) post.character
        :: List.map replyToCharacter replies
    )
        |> Maybe.Extra.values
        |> SeqDict.Extra.groupByWith Tuple.first Tuple.second


postAnnotationsFilepath : Id PostId -> String
postAnnotationsFilepath postId =
    "data/" ++ postAnnotationsFilename postId


boardAnnotationsFilepath : Id BoardId -> String
boardAnnotationsFilepath boardId =
    "data/" ++ boardAnnotationsFilename boardId


postAnnotationsFilename : Id PostId -> String
postAnnotationsFilename postId =
    "annotations-" ++ Id.toString postId ++ ".json"


boardAnnotationsFilename : Id BoardId -> String
boardAnnotationsFilename boardId =
    "annotations-board-" ++ Id.toString boardId ++ ".json"
