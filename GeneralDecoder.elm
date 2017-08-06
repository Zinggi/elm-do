module GeneralDecoder exposing (..)


type alias Decoder a b =
    a -> Result String b


run : Decoder a b -> a -> Result String b
run decoder dict =
    decoder dict


decode : b -> Decoder a b
decode b =
    (\_ -> Ok b)


succeed : b -> Decoder a b
succeed =
    decode


map : (a -> value) -> Decoder base a -> Decoder base value
map f dec =
    (\base ->
        Result.map f (dec base)
    )


{-| This allows you to run a decoder based on wheter the original decoder failed or succeded
-}
ifFailThenElse : Decoder String value -> Decoder success value -> Decoder base success -> Decoder base value
ifFailThenElse errorDecoder successDecoder decoder =
    (\base ->
        case decoder base of
            Ok v ->
                successDecoder v

            Err e ->
                errorDecoder e
    )


map2 : (a -> b -> value) -> Decoder base a -> Decoder base b -> Decoder base value
map2 f d1 d2 =
    (\base ->
        Result.map2 f
            (d1 base)
            (d2 base)
    )


andThen : (a -> Decoder base b) -> Decoder base a -> Decoder base b
andThen f dec =
    (\base ->
        Result.andThen (\a -> (f a) base) (dec base)
    )


mapError : (String -> String) -> Decoder a b -> Decoder a b
mapError f decoder =
    (\base ->
        Result.mapError f (decoder base)
    )


{-|

    This is useful in building
-}
custom : Decoder base a -> Decoder base (a -> b) -> Decoder base b
custom valDecoder continuation =
    map2 (\a f -> f a) valDecoder continuation


identityDecoder : Decoder a a
identityDecoder =
    Ok
