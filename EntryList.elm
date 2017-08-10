module EntryList exposing (..)


type alias EntryList =
    { query : String
    , entries : List Entry
    , filteredEntries : SelectionFallbackList Fallback Entry
    }


type alias Entry =
    { name : String, exec : String, comment : String }


type alias Fallback =
    String -> Entry


type alias SelectionFallbackList f e =
    { selectedElement : EntryOrFallback e f
    , entriesBefore : List e
    , entriesAfter : List e
    , fallbacksBefore : List f
    , fallbacksAfter : List f
    }


type EntryOrFallback e f
    = AnEntry e
    | AFallback f


makeEntryList : Fallback -> List Fallback -> List Entry -> EntryList
makeEntryList firstFallback fallbacks entries =
    { query = ""
    , entries = entries
    , filteredEntries = createSelectionFallbackList firstFallback fallbacks entries
    }


selectedEntry : EntryList -> Entry
selectedEntry eList =
    getCurrentSelectionF
        (\eOrF ->
            case eOrF of
                AnEntry e ->
                    e

                AFallback f ->
                    f eList.query
        )
        eList.filteredEntries


getQuery : EntryList -> String
getQuery l =
    l.query


setQuery : (String -> List Entry -> List Entry) -> String -> EntryList -> EntryList
setQuery f query eList =
    { eList | query = query, filteredEntries = updateEntriesF (f query eList.entries) eList.filteredEntries }


updateEntries : List Entry -> EntryList -> EntryList
updateEntries entries eList =
    { eList | filteredEntries = updateEntriesF entries eList.filteredEntries, entries = entries }


createSelectionFallbackList : f -> List f -> List e -> SelectionFallbackList f e
createSelectionFallbackList firstFallback fallbacks entries =
    let
        ( selEle, fallAfter, enAfter ) =
            case entries of
                e :: es ->
                    ( AnEntry e, firstFallback :: fallbacks, es )

                [] ->
                    ( AFallback firstFallback, fallbacks, [] )
    in
        { selectedElement = selEle

        -- will be stored in reverse, for easy access
        , entriesBefore = []
        , entriesAfter = enAfter

        -- will be stored in reverse, for easy access
        , fallbacksBefore = []
        , fallbacksAfter = fallAfter
        }


selectNext : EntryList -> EntryList
selectNext eList =
    { eList | filteredEntries = selectNextF eList.filteredEntries }


selectPrevious : EntryList -> EntryList
selectPrevious eList =
    { eList | filteredEntries = selectPreviousF eList.filteredEntries }


mapWithPosition : (Bool -> Entry -> a) -> EntryList -> List a
mapWithPosition f eList =
    mapWithPositionF
        (\isSelected eOrF ->
            case eOrF of
                AFallback fb ->
                    f isSelected (fb eList.query)

                AnEntry e ->
                    f isSelected e
        )
        eList.filteredEntries


selectNextF : SelectionFallbackList a b -> SelectionFallbackList a b
selectNextF list =
    case list.selectedElement of
        AnEntry e ->
            case list.entriesAfter of
                anE :: rest ->
                    { list
                        | entriesBefore = e :: list.entriesBefore
                        , selectedElement = AnEntry anE
                        , entriesAfter = rest
                    }

                [] ->
                    list

        AFallback f ->
            case list.fallbacksAfter of
                fb :: rest ->
                    { list
                        | fallbacksBefore = f :: list.fallbacksBefore
                        , selectedElement = AFallback fb
                        , fallbacksAfter = rest
                    }

                [] ->
                    list


selectPreviousF : SelectionFallbackList a b -> SelectionFallbackList a b
selectPreviousF list =
    case list.selectedElement of
        AnEntry e ->
            case list.entriesBefore of
                anE :: rest ->
                    { list
                        | entriesBefore = rest
                        , selectedElement = AnEntry anE
                        , entriesAfter = e :: list.entriesAfter
                    }

                [] ->
                    list

        AFallback f ->
            case list.fallbacksBefore of
                fb :: rest ->
                    { list
                        | fallbacksBefore = rest
                        , selectedElement = AFallback fb
                        , fallbacksAfter = fb :: list.fallbacksAfter
                    }

                [] ->
                    list


getCurrentSelectionF : (EntryOrFallback b a -> c) -> SelectionFallbackList a b -> c
getCurrentSelectionF f list =
    f list.selectedElement


updateEntriesF : List b -> SelectionFallbackList a b -> SelectionFallbackList a b
updateEntriesF entries list =
    let
        ( f1, frest ) =
            getFallbacks list
    in
        createSelectionFallbackList f1 frest entries


getFallbacks : SelectionFallbackList a b -> ( a, List a )
getFallbacks list =
    let
        unsafeHeadTail l =
            case l of
                x :: xs ->
                    ( x, xs )

                [] ->
                    Debug.crash "this shouldn't happen, as the fallback list should always have at least one element..."
    in
        case list.selectedElement of
            AFallback fb ->
                unsafeHeadTail
                    (List.reverse list.fallbacksBefore
                        ++ [ fb ]
                        ++ list.fallbacksAfter
                    )

            AnEntry _ ->
                unsafeHeadTail
                    (List.reverse list.fallbacksBefore
                        ++ list.fallbacksAfter
                    )


mapWithPositionF : (Bool -> EntryOrFallback b a -> c) -> SelectionFallbackList a b -> List c
mapWithPositionF f list =
    case list.selectedElement of
        AnEntry e ->
            (List.reverse >> List.map (AnEntry >> f False)) list.entriesBefore
                ++ [ f True (AnEntry e) ]
                ++ (List.map (AnEntry >> f False) list.entriesAfter)

        AFallback fb ->
            (List.reverse >> List.map (AFallback >> f False)) list.fallbacksBefore
                ++ [ f True (AFallback fb) ]
                ++ (List.map (AFallback >> f False) list.fallbacksAfter)
