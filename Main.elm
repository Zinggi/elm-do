module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Styles exposing (..)
import Task
import FileSystem exposing (listFiles, readFile)
import Path.Posix exposing (joinPath)
import IniParser exposing (parse)


-- update


type Msg
    = SetQuery String
    | SelectEntry String
    | DesktopEntries (Result FileSystem.Error (List Entry))


type alias Model =
    { query : String
    , entries : List Entry
    , entriesToShow : Int
    , selectedEntry : Maybe String
    }


type alias Entry =
    Result String IniParser.IniFile


initModel : Model
initModel =
    { query = ""
    , entries = []
    , entriesToShow = 5
    , selectedEntry = Nothing
    }


listFilesFullPath : String -> Task.Task FileSystem.Error (List String)
listFilesFullPath path =
    listFiles path
        |> Task.map (\names -> List.map (\name -> joinPath [ path, name ]) names)


extractEntryFromFile : String -> Entry
extractEntryFromFile content =
    -- TODO: this should actualy parse the file into an entry
    -- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-latest.html
    parse content


extractDesktopEntries : String -> Task.Task FileSystem.Error (List Entry)
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
            -- "/usr/local/share/applications"
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
    --List.filter (String.toLower >> String.contains (String.toLower query)) entries
    entries


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
                    ( { model | entries = files }, Cmd.none )

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


viewList : String -> List Entry -> Element style variation msg
viewList query entries =
    toString (filterEntries query entries)
        |> text



-- main


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subs
        , view = view
        , update = update
        }
