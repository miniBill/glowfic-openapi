module GlowficApi.Types exposing
    ( Post, Reply
    , Active(..), activeFromString, activeToString, activeVariants
    , BoardSectionsReorder_Error(..), Login_Error(..), PostsIdReplies_Error(..)
    , Int_Or_String(..)
    )

{-|


## Aliases

@docs Post, Reply


## Enum

@docs Active, activeFromString, activeToString, activeVariants


## Errors

@docs BoardSectionsReorder_Error, Login_Error, PostsIdReplies_Error


## One of

@docs Int_Or_String

-}

import OpenApi.Common
import Time


type alias Post =
    { authors : Maybe (List { id : Maybe Int, username : Maybe String })
    , board : Maybe { id : Int, name : Maybe String }
    , character :
        Maybe
            { id : Int
            , name : String
            , screenname : OpenApi.Common.Nullable String
            }
    , content : String
    , created_at : Maybe Time.Posix
    , description : Maybe String
    , icon :
        Maybe { id : Maybe Int, keyword : Maybe String, url : Maybe String }
    , id : Int
    , num_replies : Maybe Int
    , section : Maybe (OpenApi.Common.Nullable {})
    , section_order : Maybe Int
    , status : Maybe Active
    , subject : Maybe String
    , tagged_at : Maybe Time.Posix
    }


type alias Reply =
    { character :
        Maybe { id : Maybe Int, name : Maybe String, screenname : Maybe String }
    , character_name : Maybe String
    , content : Maybe String
    , created_at : Maybe Time.Posix
    , icon :
        Maybe { id : Maybe Int, keyword : Maybe String, url : Maybe String }
    , id : Maybe Int
    , updated_at : Maybe Time.Posix
    , user : Maybe { id : Maybe Int, username : Maybe String }
    }


type Active
    = Active__Active


activeFromString : String -> Maybe Active
activeFromString value =
    case value of
        "active" ->
            Just Active__Active

        _ ->
            Nothing


activeToString : Active -> String
activeToString value =
    case value of
        Active__Active ->
            "active"


activeVariants : List Active
activeVariants =
    [ Active__Active ]


type BoardSectionsReorder_Error
    = BoardSectionsReorder_401 ()
    | BoardSectionsReorder_403 ()
    | BoardSectionsReorder_404 ()
    | BoardSectionsReorder_422 ()


type Login_Error
    = Login_401 ()
    | Login_403 ()
    | Login_404 ()
    | Login_422 ()


type PostsIdReplies_Error
    = PostsIdReplies_403 ()
    | PostsIdReplies_404 ()


type Int_Or_String
    = Int_Or_String__Int Int
    | Int_Or_String__String String
