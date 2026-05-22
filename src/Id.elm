module Id exposing (Id, for, toInt, unsafe)


type Id t
    = Id Int


unsafe : Int -> Id t
unsafe =
    Id


toInt : Id t -> Int
toInt (Id i) =
    i


for : { a | id : Int } -> Id { a | id : Int }
for t =
    Id t.id
