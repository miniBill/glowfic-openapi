module Glowfic.Utils exposing (allCharactersIds, annotationsFilename, annotationsFilepath)

import GlowficApi.Types exposing (PostDetails, Reply)
import Id exposing (CharacterId, Id, PostId)
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


annotationsFilepath : Id PostId -> String
annotationsFilepath postId =
    "data/" ++ annotationsFilename postId


annotationsFilename : Id PostId -> String
annotationsFilename postId =
    "annotations-" ++ Id.toString postId ++ ".json"
