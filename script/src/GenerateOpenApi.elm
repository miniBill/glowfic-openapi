module GenerateOpenApi exposing (run)

import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import Cli
import Common
import Elm
import Elm.Annotation
import FatalError exposing (FatalError)
import Gen.Json.Decode
import Gen.Json.Encode
import Json.Encode
import OpenApi.Config
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Cli.withConfig config


config : OpenApi.Config.Config
config =
    OpenApi.Config.init "generated"
        |> OpenApi.Config.withInput
            (OpenApi.Config.inputFrom (OpenApi.Config.File "glowfic-openapi.yaml")
                |> OpenApi.Config.withEffectTypes
                    [ OpenApi.Config.DillonkearnsElmPagesTask
                    , OpenApi.Config.DillonkearnsElmPagesTaskRecord
                    ]
            )
        |> OpenApi.Config.withFormats
            (\found ->
                found
                    |> List.filterMap
                        (\{ basicType, format } ->
                            case ( basicType, String.split "Id " format ) of
                                ( Common.Integer, [ "", name ] ) ->
                                    let
                                        annotation : Elm.Annotation.Annotation
                                        annotation =
                                            Elm.Annotation.namedWith
                                                [ "Id" ]
                                                "Id"
                                                [ Elm.Annotation.named [ "Id" ] name ]
                                    in
                                    { annotation = annotation
                                    , basicType = basicType
                                    , decoder =
                                        Gen.Json.Decode.call_.map
                                            (Elm.value
                                                { importFrom = [ "Id" ]
                                                , annotation =
                                                    Elm.Annotation.function
                                                        [ Elm.Annotation.int ]
                                                        annotation
                                                        |> Just
                                                , name = "unsafe"
                                                }
                                            )
                                            Gen.Json.Decode.int
                                    , encode =
                                        \id ->
                                            Gen.Json.Encode.call_.int
                                                (Elm.apply
                                                    (Elm.value
                                                        { importFrom = [ "Id" ]
                                                        , annotation =
                                                            Elm.Annotation.function
                                                                [ annotation ]
                                                                Elm.Annotation.int
                                                                |> Just
                                                        , name = "toInt"
                                                        }
                                                    )
                                                    [ id ]
                                                )
                                    , toParamString =
                                        \id ->
                                            Elm.apply
                                                (Elm.value
                                                    { importFrom = [ "Id" ]
                                                    , annotation =
                                                        Elm.Annotation.function
                                                            [ annotation ]
                                                            Elm.Annotation.int
                                                            |> Just
                                                    , name = "toString"
                                                    }
                                                )
                                                [ id ]
                                    , example = Json.Encode.int 2
                                    , format = format
                                    , requiresPackages = []
                                    , sharedDeclarations = []
                                    }
                                        |> Just

                                _ ->
                                    Nothing
                        )
            )
