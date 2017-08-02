module FileSystem exposing (..)

import Task exposing (Task)
import Native.FileSystem


type alias Error =
    String


listFiles : String -> Task Error (List String)
listFiles =
    Native.FileSystem.listFiles
