module ChildProcess exposing (..)

import Task exposing (Task)
import Native.ChildProcess


type alias Output =
    { stdOut : String, stdErr : String }


executeCommand : String -> Task String Output
executeCommand =
    Native.ChildProcess.executeCommand
