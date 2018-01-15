port module Utils.CommonFunctions exposing (..)

import Char exposing (isLower, isUpper)
import Date
import Date.Extra
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Utils.CommonTypes exposing (DropdownItem)
import Set exposing (Set)
import Http


port displaySuccessMessage : String -> Cmd msg


port displayErrorMessage : String -> Cmd msg


port setLoadingStatus : Bool -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


defaultMaybeMsg : (Maybe String -> msg) -> String -> msg
defaultMaybeMsg fromMaybe input =
    fromMaybe (Just input)


defaultString : Maybe String -> String
defaultString str =
    Maybe.withDefault "" str


defaultLower : Maybe String -> String
defaultLower str =
    String.toLower (defaultString str)


defaultBool : Maybe Bool -> Bool
defaultBool bool =
    Maybe.withDefault False bool


defaultInt : Maybe Int -> Int
defaultInt int =
    Maybe.withDefault -1 int


defaultIntToString : Maybe Int -> String
defaultIntToString int =
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
        Ok t ->
            dateToString t

        Err _ ->
            ""


defaultDateTime : Maybe String -> String
defaultDateTime str =
    dateTime (defaultString str)


defaultDate : Maybe String -> String
defaultDate str =
    date (defaultString str)


defaultLowerDate : Maybe String -> String
defaultLowerDate str =
    String.toLower (date (defaultString str))


defaultLowerDateTime : Maybe String -> String
defaultLowerDateTime str =
    String.toLower (dateTime (defaultString str))


defaultIntStr : String -> Int
defaultIntStr str =
    case String.toInt str of
        Ok t ->
            t

        Err _ ->
            0


maybeVal : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeVal encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null


maybeToDateString : Maybe String -> Maybe String
maybeToDateString maybeDateStr =
    case maybeDateStr of
        Just dateStr ->
            case Date.fromString dateStr of
                Ok date ->
                    Just (Date.Extra.toUtcIsoString date)

                Err _ ->
                    Nothing

        Nothing ->
            Nothing


decodeDropdownItem : Decode.Decoder DropdownItem
decodeDropdownItem =
    Pipeline.decode DropdownItem
        |> Pipeline.required "Id" (Decode.maybe Decode.int)
        |> Pipeline.required "Name" Decode.string


uniqueBy : (a -> comparable) -> List a -> List a
uniqueBy f list =
    uniqueHelp f Set.empty list []


uniqueHelp : (a -> comparable) -> Set comparable -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
                if Set.member computedFirst existing then
                    uniqueHelp f existing rest accumulator
                else
                    uniqueHelp f (Set.insert computedFirst existing) rest (first :: accumulator)


postRequest : Encode.Value -> String -> Http.Request String
postRequest body url =
    Http.request
        { body = body |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = url
        , withCredentials = False
        }


getResponseError : String -> Maybe String
getResponseError str =
    case Decode.decodeString (Decode.field "Error" Decode.int) str of
        Ok _ ->
            case Decode.decodeString (Decode.field "Message" Decode.string) str of
                Ok t ->
                    Just t

                Err _ ->
                    Just ""

        Err _ ->
            Nothing
