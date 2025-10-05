module DownloadLinks exposing (run)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions task


task : BackendTask FatalError ()
task =
    Debug.todo "TODO"
