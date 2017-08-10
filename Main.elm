module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Element.Attributes as A
import Styles exposing (..)
import Task
import Keyboard
import Keyboard.Key exposing (fromCode, Key(Enter, Down, Up))
import Path.Posix exposing (joinPath)


--

import FileSystem exposing (listFiles, readFile)
import ChildProcess exposing (executeCommand)


--

import IniParser exposing (parse)
import DictDecoder exposing (decode, run, at, required, optional, string)
import EntryList
    exposing
        ( EntryList
        , Entry
        , makeEntryList
        , mapWithPosition
        , selectPrevious
        , selectNext
        , selectedEntry
        , setQuery
        , getQuery
        , updateEntries
        )


-- update


type Msg
    = SetQuery String
    | DesktopEntries (Result FileSystem.Error (List (Result String Entry)))
    | KeyDown Keyboard.KeyCode


type alias Model =
    { entries : EntryList
    , errors : List String
    }


cmdFallback : String -> Entry
cmdFallback query =
    { name = "Cmd", exec = query, comment = "> " ++ query }


initModel : Model
initModel =
    { entries = makeEntryList cmdFallback [] []
    , errors = []
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
                        decode identity
                            |> required "Desktop Entry"
                                (decode EntryList.Entry
                                    |> required "Name" string
                                    |> optional "Exec" string ""
                                    |> optional "Comment" string ""
                                )

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
                    run decoder dict
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


startApp : Entry -> Cmd Msg
startApp entry =
    -- TODO: make it a Task Error () and handle error
    Task.attempt identity (executeCommand entry.exec)
        |> Cmd.map (\_ -> KeyDown -1)


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
    List.filter
        (\entry ->
            String.toLower entry.name
                |> String.contains (String.toLower query)
                |> (||) (String.toLower entry.comment |> String.contains (String.toLower query))
        )
        entries


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ entries } as model) =
    case msg of
        SetQuery query ->
            ( { model | entries = EntryList.setQuery filterEntries query entries }, Cmd.none )

        KeyDown code ->
            case fromCode code of
                Enter ->
                    -- start app
                    ( model, startApp (selectedEntry entries) )

                Up ->
                    ( { model | entries = selectPrevious model.entries }, Cmd.none )

                Down ->
                    ( { model | entries = selectNext model.entries }, Cmd.none )

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
                        ( { model | entries = updateEntries entries model.entries, errors = errors }, Cmd.none )

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
        [ inputText None [ E.onInput SetQuery ] (getQuery model.entries)
        , viewList model.entries
        , (toString >> text) model.errors
        ]


viewList : EntryList -> Element Styles Variations msg
viewList entries =
    column Base
        []
        (entries
            |> mapWithPosition
                (\isSelected entry ->
                    row Styles.Entry
                        [ A.vary Selected isSelected ]
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
