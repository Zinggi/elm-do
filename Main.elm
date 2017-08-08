module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Element.Attributes as A
import Styles exposing (..)
import Task
import Keyboard
import Keyboard.Key exposing (fromCode, Key(Enter))
import Path.Posix exposing (joinPath)


--

import FileSystem exposing (listFiles, readFile)
import ChildProcess exposing (executeCommand)


--

import IniParser exposing (parse)
import DictDecoder exposing (decode, run, at, required, optional, string)


-- update


type Msg
    = SetQuery String
    | DesktopEntries (Result FileSystem.Error (List (Result String Entry)))
    | KeyDown Keyboard.KeyCode


type alias Model =
    { query : String

    -- TODO: this shouldn't be a list.
    -- There should always be a fallback option, e.g. search on google
    -- this would make the selection easier, as there would always be a selected entry
    , entries : List Entry
    , errors : List String
    , entriesToShow : Int
    , selectedEntry : Selection
    }


type Selection
    = Fallback String
    | AnEntry Entry
    | EmptyQuery


type alias Entry =
    { name : String, exec : String, comment : String }


initModel : Model
initModel =
    { query = ""
    , entries = []
    , errors = []
    , entriesToShow = 5
    , selectedEntry = EmptyQuery
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
                            |> optional "Exec" string ""
                            |> optional "Comment" string ""

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
                    |> List.filter (String.endsWith ".desktop")
                    |> List.map
                        (\file ->
                            readFile file
                                |> Task.map extractEntryFromFile
                                |> Task.map
                                    (Result.mapError
                                        (\err ->
                                            "Error in file: '" ++ file ++ "':\n" ++ err
                                        )
                                    )
                        )
                    |> Task.sequence
            )


startApp : Selection -> Cmd Msg
startApp selection =
    case selection of
        AnEntry e ->
            -- TODO: make it a Task Error () and handle error
            Task.attempt identity (executeCommand e.exec)
                |> Cmd.map (\_ -> KeyDown -1)

        Fallback cmd ->
            -- TODO: should provide options, e.g. google, cmd, etc..
            Task.attempt identity (executeCommand cmd)
                |> Cmd.map (\_ -> KeyDown -1)

        _ ->
            Cmd.none


init : ( Model, Cmd Msg )
init =
    let
        path =
            -- "~/.local/share/applications" -- needs more work, as ~ is a side effect
            -- "/usr/share/applications"
            "/usr/local/share/applications"
    in
        ( initModel
        , Task.attempt DesktopEntries (extractDesktopEntries path)
        )


filterEntries : String -> List Entry -> List Entry
filterEntries query entries =
    -- TODO: correct filtering
    List.filter
        (\entry ->
            String.toLower entry.name
                |> String.contains (String.toLower query)
                |> (||) (String.toLower entry.comment |> String.contains (String.toLower query))
        )
        entries


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ query, entries, entriesToShow, selectedEntry } as model) =
    case msg of
        SetQuery query ->
            let
                newSelectedEntry =
                    filterEntries query entries
                        |> List.head
                        |> Maybe.map AnEntry
                        |> Maybe.withDefault
                            (if query == "" then
                                EmptyQuery
                             else
                                Fallback query
                            )
            in
                ( { model | query = query, selectedEntry = newSelectedEntry }, Cmd.none )

        KeyDown code ->
            case fromCode code of
                Enter ->
                    -- start app
                    ( model, startApp selectedEntry )

                _ ->
                    ( model, Cmd.none )

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


subs : a -> Sub Msg
subs model =
    Keyboard.downs KeyDown



-- view


view : Model -> Html Msg
view model =
    viewport stylesheet (mainView model)


mainView : Model -> Element Styles Variations Msg
mainView model =
    column None
        []
        [ inputText None [ E.onInput SetQuery ] model.query
        , viewList model.query model.selectedEntry model.entries
        , (toString >> text) model.errors
        ]


viewList : String -> Selection -> List Entry -> Element Styles Variations msg
viewList query selection entries =
    column Base
        []
        (filterEntries query entries
            |> List.map
                (\entry ->
                    row Styles.Entry
                        [ A.vary Selected (selection == AnEntry entry) ]
                        [ circle 10 None [] (text "")
                        , column None
                            []
                            [ bold entry.name
                            , italic entry.exec
                            , text entry.comment
                            ]
                        ]
                )
            |> List.intersperse (hairline None)
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
