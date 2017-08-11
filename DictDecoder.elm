module DictDecoder exposing (..)

{-|


# Dict Decoder

Allows to decode nested `Dict`s into a more structured elm datastructure.


## Example

    import Dict exposing (Dict)
    import DictDecoder exposing (DictDecoder, required, optional, float, int)

    exampleDecoder : DictDecoder ( Float, Int )
    exampleDecoder =
        decode (,)
            |> required "test" float
            |> optional "opt" int 0


    Dict.fromList [ ( "test", "-11.23" ), ( "opt", "20" ), ( "bla", "aasa" ) ]
        |> run exampleDecoder

    --> (-11.23, 20)

-}

import Dict exposing (Dict)
import Json.Decode as JD
import GeneralDecoder as GD exposing (..)


type alias ValueDecoder value =
    Decoder String String value


type alias DictDecoder comparable value target =
    Decoder String (Dict comparable value) target


decode : target -> Decoder String source target
decode =
    GD.decode


run : Decoder String source target -> source -> Result String target
run =
    GD.run


float : ValueDecoder Float
float =
    JD.decodeString JD.float


int : ValueDecoder Int
int =
    JD.decodeString JD.int


{-| Decodes some text.
If you want to only decode text inside quotes, see `string`
This decoder never fails.

    run text "some text"
    --> Ok "some text"

-}
text : ValueDecoder String
text =
    Ok


{-| Decodes a string.
Needs to be in double quotes.
For bare words, see `text`

    run string "\"some string\""
    --> Ok "some string"

    run string "'single quotes'"
    --> Err "TODO"

-}
string : ValueDecoder String
string =
    JD.decodeString JD.string


{-| -}
at : comparable -> Decoder String source target -> DictDecoder comparable source target
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


{-| -}
required : comparable -> Decoder String source target -> DictDecoder comparable source (target -> b) -> DictDecoder comparable source b
required key valDecoder =
    custom (at key valDecoder)


optional : comparable -> Decoder String source target -> target -> DictDecoder comparable source (target -> b) -> DictDecoder comparable source b
optional key valDecoder defaultValue =
    custom
        (at key identityDecoder
            -- in case `at` fails, succed with default
            -- in case `at` succeeds, try valDecoder
            |> ifFailThenElse (succeed defaultValue) (valDecoder)
            |> mapError (\err -> "Error at field '" ++ toString key ++ "':\n" ++ err)
        )
