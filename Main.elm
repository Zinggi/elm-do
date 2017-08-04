module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Element.Attributes as A
import Styles exposing (..)
import Task
import FileSystem exposing (listFiles, readFile)
import Path.Posix exposing (joinPath)


-- update


type Msg
    = SetQuery String
    | SelectEntry String
    | DesktopEntries (Result FileSystem.Error (List String))
    | GetFileContent (Result FileSystem.Error String)


type alias Model =
    { query : String
    , entries : List Entry
    , entriesToShow : Int
    , selectedEntry : Maybe String
    , content : String
    }


type alias Entry =
    String


initModel : Model
initModel =
    { query = ""
    , entries = [ "test", "lol", "motherfucker", "etf", "loser", "hitler", "adolf" ]
    , entriesToShow = 5
    , selectedEntry = Nothing
    , content = ""
    }


init =
    -- /usr/local/share/applications
    -- ~/.local/share/applications
    -- /usr/share/applications
    let
        path =
            "/usr/share/applications"
    in
        ( initModel
        , Task.attempt DesktopEntries
            (listFiles path
                |> Task.map (\names -> List.map (\name -> joinPath [ path, name ]) names)
            )
        )


filterEntries : String -> List Entry -> List Entry
filterEntries query entries =
    List.filter (String.toLower >> String.contains (String.toLower query)) entries


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
                    ( { model | entries = files }, Task.attempt GetFileContent (readFile (List.head files |> Maybe.withDefault "/")) )

                Err err ->
                    Debug.crash err

        GetFileContent res ->
            case res of
                Ok content ->
                    ( { model | content = content }, Cmd.none )

                Err error ->
                    Debug.crash error


subs model =
    Sub.none



-- view


view : Model -> Html Msg
view model =
    viewport stylesheet (mainView model)


mainView model =
    column None
        []
        [ inputText None [ E.onInput SetQuery ] model.query
        , viewList model.query model.entries
        , text model.content
        ]


viewList query entries =
    toString (filterEntries query entries)
        |> text



-- main


main =
    program
        { init = init
        , subscriptions = subs
        , view = view
        , update = update
        }
