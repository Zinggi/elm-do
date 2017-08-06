module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Styles exposing (..)
import Task
import Dict
import Result.Extra as Result


--

import FileSystem exposing (listFiles, readFile)
import Path.Posix exposing (joinPath)
import IniParser exposing (parse)
import DictDecoder exposing (decode, run, at, required, string)


-- update


type Msg
    = SetQuery String
    | SelectEntry String
    | DesktopEntries (Result FileSystem.Error (List (Result String Entry)))


type alias Model =
    { query : String
    , entries : List Entry
    , errors : List String
    , entriesToShow : Int
    , selectedEntry : Maybe String
    }


type alias Entry =
    { name : String, exec : String }


initModel : Model
initModel =
    { query = ""
    , entries = []
    , errors = []
    , entriesToShow = 5
    , selectedEntry = Nothing
    }


listFilesFullPath : String -> Task.Task FileSystem.Error (List String)
listFilesFullPath path =
    listFiles path
        |> Task.map (\names -> List.map (\name -> joinPath [ path, name ]) names)


extractEntryFromFile : String -> Result String Entry
extractEntryFromFile content =
    -- TODO: this should actualy parse the file into an entry
    -- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html
    parse content
        |> Result.andThen
            (\dict ->
                let
                    decoder =
                        decode Entry
                            |> required "Name" string
                            |> required "Exec" string

                    -- [Desktop Entry]
                    -- Type=Application
                    -- Name=Foo Viewer
                    -- Comment=The best viewer for Foo objects available!
                    -- TryExec=fooview
                    -- Exec=fooview %F
                    -- Icon=fooview
                    -- MimeType=image/x-foo;
                    -- Actions=Gallery;Create;
                in
                    run (at "Desktop Entry" decoder) dict
            )


extractDesktopEntries : String -> Task.Task FileSystem.Error (List (Result String Entry))
extractDesktopEntries path =
    listFilesFullPath path
        |> Task.andThen
            (\files ->
                files
                    |> List.map
                        (\file ->
                            readFile file
                                |> Task.map extractEntryFromFile
                        )
                    |> Task.sequence
            )


init : ( Model, Cmd Msg )
init =
    let
        path =
            -- "~/.local/share/applications" -- needs more work, as ~ is a side effect
            -- "/usr/local/share/applications"
            "/usr/share/applications"
    in
        ( initModel
        , Task.attempt DesktopEntries (extractDesktopEntries path)
        )


filterEntries : String -> List Entry -> List Entry
filterEntries query entries =
    -- TODO: correct filtering
    List.filter (.name >> String.toLower >> String.contains (String.toLower query)) entries


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ query, entries, entriesToShow } as model) =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }, Cmd.none )

        SelectEntry id ->
            ( { model | selectedEntry = Just id, query = id }, Cmd.none )

        DesktopEntries res ->
            case res of
                Ok files ->
                    let
                        ( entries, errors ) =
                            List.foldl
                                (\val ( l, r ) ->
                                    case val of
                                        Ok v ->
                                            ( v :: l, r )

                                        Err e ->
                                            ( l, e :: r )
                                )
                                ( [], [] )
                                files
                    in
                        ( { model | entries = entries, errors = errors }, Cmd.none )

                Err err ->
                    Debug.crash err


subs : a -> Sub msg
subs model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    viewport stylesheet (mainView model)


mainView : Model -> Element Styles variation Msg
mainView model =
    column None
        []
        [ inputText None [ E.onInput SetQuery ] model.query
        , viewList model.query model.entries
        ]


viewList : String -> List Entry -> Element Styles variation msg
viewList query entries =
    column None
        []
        (filterEntries query entries
            |> List.map
                (\entry ->
                    row None
                        []
                        [ circle 10 None [] (text "")
                        , column None
                            []
                            [ bold entry.name
                            , text entry.exec
                            ]
                        ]
                )
        )



-- main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subs
        , view = view
        , update = update
        }
