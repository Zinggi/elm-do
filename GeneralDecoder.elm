module GeneralDecoder exposing (..)

{-| TODO:
run elm-doc-test


# General Decoder

A decoder is an object that describes how to translate something of type `source`
to something of type `target`, with the possibility of an error.

@docs Decoder

Note that any `Json.Decode.Decoder` can be transformed into a GeneralDecoder.Decoder
by partially applying `decodeString : Decoder a -> String -> Result String a`,
e.g.

    import Json.Decode as JD
    import GeneralDecoder as GD

    listDecoder : GD.Decoder String String (List Int)
    listDecoder =
        JD.decodeString (JD.list JD.int)

-}


{-| A decoder is just an alias for a this function
-}
type alias Decoder error source target =
    source -> Result error target


{-| Run a decoder
-}
run : Decoder error source target -> source -> Result error target
run decoder dict =
    decoder dict


{-| A decoder that always succeeds.
Same as succeed, but reads nicer in a pipeline.
-}
decode : target -> Decoder error source target
decode b =
    (\_ -> Ok b)


{-| A decoder that always succeeds.
Same as decode
-}
succeed : target -> Decoder error source target
succeed =
    decode


{-| Transform the output of a decoder to some other value
-}
map : (a -> b) -> Decoder error source a -> Decoder error source b
map f dec =
    (\source ->
        Result.map f (dec source)
    )


{-| This allows you to run a decoder based on wheter the original decoder failed or succeded
-}
ifFailThenElse : Decoder error error target -> Decoder error success target -> Decoder error source success -> Decoder error source target
ifFailThenElse errorDecoder successDecoder decoder =
    (\source ->
        case decoder source of
            Ok v ->
                successDecoder v

            Err e ->
                errorDecoder e
    )


{-| Map with two decoders
-}
map2 : (a -> b -> target) -> Decoder error source a -> Decoder error source b -> Decoder error source target
map2 f d1 d2 =
    (\source ->
        Result.map2 f
            (d1 source)
            (d2 source)
    )


{-| Run a decoder based on the result of a previous decoder
-}
andThen : (a -> Decoder error source b) -> Decoder error source a -> Decoder error source b
andThen f dec =
    (\source ->
        Result.andThen (\a -> (f a) source) (dec source)
    )


{-| Transform the error
-}
mapError : (errorA -> errorB) -> Decoder errorA source target -> Decoder errorB source target
mapError f decoder =
    (\source ->
        Result.mapError f (decoder source)
    )


{-| This is useful in building a pipeline friendly api, e.g.

    required field valueDecoder =
        custom (at field valueDecoder)

    Defines the same decoder as the `required` decoder from `NoRedInk/elm-decode-pipeline`

-}
custom : Decoder error source a -> Decoder error source (a -> b) -> Decoder error source b
custom valDecoder continuation =
    map2 (\a f -> f a) valDecoder continuation


{-| A decoder that does nothing. Can be usefull when combining with map, etc.
-}
identityDecoder : Decoder error a a
identityDecoder =
    Ok
