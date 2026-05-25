module Id exposing
    ( Id, for, toInt, toString, unsafe
    , AliasId, BoardId, BookmarkId, CharacterId, GalleryId, IconId, PostId, ReplyId, SectionId, TagId, TemplateId, UserId
    , codec
    )

{-|

@docs Id, for, toInt, toString, unsafe
@docs AliasId, BoardId, BookmarkId, CharacterId, GalleryId, IconId, PostId, ReplyId, SectionId, TagId, TemplateId, UserId

-}

import Codec exposing (Codec)


type Id t
    = Id Int


type AliasId
    = AliasId Never


type BoardId
    = BoardId Never


type BookmarkId
    = BookmarkId Never


type CharacterId
    = CharacterId Never


type GalleryId
    = GalleryId Never


type IconId
    = IconId Never


type PostId
    = PostId Never


type ReplyId
    = ReplyId Never


type SectionId
    = SectionId Never


type TagId
    = TagId Never


type TemplateId
    = TemplateId Never


type UserId
    = UserId Never


unsafe : Int -> Id t
unsafe =
    Id


toInt : Id t -> Int
toInt (Id i) =
    i


for : { a | id : Id t } -> Id t
for t =
    t.id


toString : Id t -> String
toString (Id i) =
    String.fromInt i


codec : Codec (Id t)
codec =
    Codec.map unsafe toInt Codec.int
