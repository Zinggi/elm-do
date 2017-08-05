module FileSystem exposing (..)

import Task exposing (Task)
import Path.Posix exposing (joinPath)
import Native.FileSystem


type alias Error =
    String


type alias Description =
    { type_ : FileType }


type FileType
    = IsFile
    | IsDirectory


readDirectory : String -> Task Error (List String)
readDirectory =
    Native.FileSystem.readDirectory


description : String -> Task Error Description
description =
    Native.FileSystem.description


type alias Options =
    { encoding : Maybe String }


readFileWith : Options -> String -> Task Error String
readFileWith =
    Native.FileSystem.readFile


readFile : String -> Task Error String
readFile =
    readFileWith { encoding = Just "utf8" }


{-| TODO: returns error if symbolic link present, as fs.stat doesn't work on symbolic links
-}
listFiles : String -> Task Error (List String)
listFiles dir =
    readDirectory dir
        |> Task.andThen
            (\files ->
                taskFilter
                    (\f ->
                        description (joinPath [ dir, f ])
                            |> Task.map (\description -> description.type_ == IsFile)
                    )
                    files
            )


taskFilter : (a -> Task err Bool) -> List a -> Task err (List a)
taskFilter f list =
    list
        |> List.map
            (\li ->
                f li |> Task.map (\res -> ( res, li ))
            )
        |> Task.sequence
        |> Task.map
            (\res ->
                List.filterMap
                    (\( ret, value ) ->
                        if ret then
                            Just value
                        else
                            Nothing
                    )
                    res
            )
