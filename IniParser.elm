module IniParser exposing (..)

{-| <https://en.wikipedia.org/wiki/INI_file>
-}

import Parser exposing (..)
import Char
import Dict exposing (Dict)


--

import Html
import Html.Attributes as A
import Html.Events as E


type alias IniFile =
    Dict.Dict SectionHeader (Dict.Dict Key Value)


type alias SectionHeader =
    String


type alias Key =
    String


type alias Value =
    String



-- parse : String -> Result String IniFile


parse text =
    Parser.run file text
        |> Result.mapError toString


file : Parser IniFile
file =
    succeed Dict.fromList
        |= repeat zeroOrMore
            (section
                |. (repeat zeroOrMore ignoredStuff)
            )
        |. end


section : Parser ( SectionHeader, Dict Key Value )
section =
    inContext "section" <|
        delayedCommitMap (\h kvs -> ( h, Dict.fromList kvs ))
            (sectionHeader
                |. repeat oneOrMore ignoredStuff
            )
            (repeat zeroOrMore
                (keyValue
                    |. repeat zeroOrMore ignoredStuff
                )
            )


sectionHeaderText : Parser String
sectionHeaderText =
    keep (oneOrMore) (\c -> c /= ']')


spaces : Parser ()
spaces =
    ignore zeroOrMore isSpace


isSpace : Char -> Bool
isSpace c =
    c == ' '


key : Parser Key
key =
    succeed (++)
        |= keep (Exactly 1) (\c -> c /= '[')
        |= keep (zeroOrMore) (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\n' && c /= '=')


value : Parser Value
value =
    keep (zeroOrMore) (\c -> c /= '\x0D' && c /= '\n')


{-| name=value
-}
keyValue : Parser ( Key, Value )
keyValue =
    inContext "key-value pair" <|
        delayedCommitMap (,)
            key
            (succeed identity
                |. spaces
                |. symbol "="
                |. spaces
                |= value
            )


{-| [section]
-}
sectionHeader : Parser SectionHeader
sectionHeader =
    inContext "sectionHeader" <|
        delayedCommit (symbol "[")
            (succeed identity
                |= sectionHeaderText
                |. symbol "]"
            )


ignoredStuff : Parser ()
ignoredStuff =
    oneOf
        [ -- ignore windows line endings
          -- '\x0D' is \r, elm format somehow thought \x0D was nicer..
          delayedCommit (ignore (Exactly 1) (\c -> c == '#' || c == ';' || isSpace c || c == '\x0D')) (ignoreUntil "\n")
        , ignore (Exactly 1) (\c -> c == '\n')
        ]


main =
    Html.beginnerProgram
        { model = ""
        , view = view
        , update = \msg model -> msg
        }


view model =
    Html.div []
        [ Html.textarea [ A.value model, E.onInput identity ] []
        , Html.div [] [ Html.text (toString (parse model)) ]
        ]
