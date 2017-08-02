module Main exposing (main)

import Html exposing (Html, program)
import Element exposing (..)
import Element.Events as E
import Element.Attributes as A
import Styles exposing (..)
import Task
import FileSystem exposing (listFiles)


-- update


type Msg
    = SetQuery String
    | SelectEntry String
    | DesktopEntries (Result FileSystem.Error (List String))


type alias Model =
    { query : String
    , entries : List Entry
    , entriesToShow : Int
    , selectedEntry : Maybe String
    }


type alias Entry =
    String


initModel : Model
initModel =
    { query = ""
    , entries = [ "test", "lol", "motherfucker", "etf", "loser", "hitler", "adolf" ]
    , entriesToShow = 5
    , selectedEntry = Nothing
    }


init =
    -- /usr/local/share/applications
    -- ~/.local/share/applications
    -- /usr/share/applications
    ( initModel, Task.attempt DesktopEntries (listFiles "/usr/share/applications") )


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
                    ( { model | entries = files }, Cmd.none )

                Err err ->
                    Debug.crash err


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
