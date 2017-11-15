module Utils.CommonFunctions exposing (..)

import Char exposing (isLower, isUpper)
import Date
import Date.Extra


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


defaultString : Maybe String -> String
defaultString str =
    Maybe.withDefault "" str


defaultBool : Maybe Bool -> Bool
defaultBool bool =
    Maybe.withDefault False bool


defaultInt : Maybe Int -> String
defaultInt int =
    case int of
        Just t ->
            toString t

        Nothing ->
            ""


dateTimeToString : Date.Date -> String
dateTimeToString date =
    Date.Extra.toFormattedString "MM/dd/yyyy hh:mm:ss a" date


dateToString : Date.Date -> String
dateToString date =
    Date.Extra.toFormattedString "MM/dd/yyyy" date


dateTime : String -> String
dateTime str =
    case Date.fromString str of
        Ok date ->
            dateTimeToString date

        Err _ ->
            ""


date : String -> String
date str =
    case Date.fromString str of
        Ok date ->
            dateToString date

        Err _ ->
            ""


defaultDateTime : Maybe String -> String
defaultDateTime str =
    dateTime (defaultString str)


defaultDate : Maybe String -> String
defaultDate str =
    date (defaultString str)


defaultIntStr : String -> Int
defaultIntStr str =
    case String.toInt str of
        Ok t ->
            t

        Err _ ->
            0
