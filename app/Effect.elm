module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)


type Effect msg
    = Batch (List (Effect msg))
    | Cmd (Cmd msg)


none : Effect msg
none =
    Batch []


batch : List (Effect msg) -> Effect msg
batch =
    Batch


fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


map : (a -> b) -> Effect a -> Effect b
map f effect =
    case effect of
        Batch children ->
            Batch (List.map (map f) children)

        Cmd cmd ->
            Cmd (Cmd.map f cmd)


perform : { a | fromPageMsg : pageMsg -> msg } -> Effect pageMsg -> Cmd msg
perform cfg effect =
    case effect of
        Cmd cmd ->
            Cmd.map cfg.fromPageMsg cmd

        Batch effects ->
            Cmd.batch (List.map (perform cfg) effects)
