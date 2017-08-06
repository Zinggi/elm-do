module DictDecoder exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
import GeneralDecoder as GD exposing (..)


decode : b -> Decoder a b
decode =
    GD.decode


run : Decoder a b -> a -> Result String b
run =
    GD.run


float : Decoder String Float
float =
    JD.decodeString JD.float


int : Decoder String Int
int =
    JD.decodeString JD.int


string : Decoder String String
string =
    Ok


example =
    Dict.fromList [ ( "test", "-11.23" ), ( "opt", "20" ), ( "bla", "aasa" ) ]
        |> run exampleDecoder


exampleDecoder : Decoder (Dict String String) ( Float, Int )
exampleDecoder =
    decode (,)
        |> required "test" float
        |> optional "opt" int 0


{-| -required : comparable -> Decoder a b -> Decoder (Dict comparable a) (b -> c) -> Decoder (Dict comparable b) c
-}
at : comparable -> Decoder a value -> Decoder (Dict comparable a) value
at key valDecoder =
    (\dict ->
        case Dict.get key dict of
            Just v ->
                valDecoder v
                    |> Result.mapError
                        (\orig ->
                            "Error at field '" ++ toString key ++ "':\n" ++ orig
                        )

            Nothing ->
                Err ("Field '" ++ toString key ++ "' not found!")
    )


required : comparable -> Decoder a value -> Decoder (Dict comparable a) (value -> b) -> Decoder (Dict comparable a) b
required key valDecoder =
    custom (at key valDecoder)


optional : comparable -> Decoder value a -> a -> Decoder (Dict comparable value) (a -> b) -> Decoder (Dict comparable value) b
optional key valDecoder defaultValue =
    custom
        (at key identityDecoder
            -- in case `at` fails, succed with default
            -- in case `at` succeeds, try valDecoder
            |> ifFailThenElse (succeed defaultValue) (valDecoder)
            |> mapError (\err -> "Error at field '" ++ toString key ++ "':\n" ++ err)
        )
