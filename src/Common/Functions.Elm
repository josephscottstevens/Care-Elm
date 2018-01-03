port module Common.Functions
    exposing
        ( setLoadingStatus
        , displaySuccessMessage
        , displayErrorMessage
        , error
        , isAlpha
        , defaultMaybeMsg
        , defaultString
        , defaultLower
        , defaultBool
        , defaultInt
        , defaultIntToString
        , dateTimeToString
        , dateToString
        , dateTime
        , date
        , defaultDateTime
        , defaultDate
        , defaultLowerDate
        , defaultLowerDateTime
        , defaultIntStr
        , maybeVal
        , maybeToDateString
        , maybeStringToInt
        , getResponseError
        , decodeDropdownItem
        , postRequest
        , getDesc
        , getRecordTypeById
        , getId
        , sendMenuMessage
        , setUnsavedChanges
        , deletePrompt
        , deleteConfirmed
        )

import Char exposing (isLower, isUpper)
import Date
import Date.Extra
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, maybe, string, int, field, decodeString)
import Http
import Json.Decode.Pipeline exposing (required, decode)
import Http
import Common.Types
    exposing
        ( DropdownItem
        , AddEditDataSource
        , MenuMessage
        , RecordType
            ( PrimaryCare
            , Specialty
            , Labs
            , Radiology
            , Hospitalizations
            , Legal
            , CallRecordings
            , PreviousHistories
            , Enrollment
            , Misc
            )
        )
import Common.Route as Route exposing (Route)


port setLoadingStatus : Bool -> Cmd msg


port displaySuccessMessage : String -> Cmd msg


port displayErrorMessage : String -> Cmd msg


port sendMenuMessage : MenuMessage -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port deletePrompt : Int -> Cmd msg


port deleteConfirmed : (Int -> msg) -> Sub msg


error : Http.Error -> Maybe Route
error httpError =
    Just (Route.Error (toString httpError))


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


maybeStringToInt : String -> Maybe Int
maybeStringToInt str =
    case String.toInt str of
        Ok t ->
            Just t

        Err _ ->
            Nothing


getResponseError : String -> Maybe String
getResponseError str =
    case decodeString (field "Error" int) str of
        Ok _ ->
            case decodeString (field "Message" string) str of
                Ok t ->
                    Just t

                Err _ ->
                    Just ""

        Err _ ->
            Nothing



-- Dropdown Helpers


decodeDropdownItem : Decoder DropdownItem
decodeDropdownItem =
    decode DropdownItem
        |> required "Id" (maybe Decode.int)
        |> required "Name" Decode.string



-- Http helpers


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



-- Record helpers


getDesc : RecordType -> String
getDesc recordType =
    case recordType of
        PrimaryCare ->
            "Primary Care Records"

        Specialty ->
            "Specialty Records"

        Labs ->
            "Lab Records"

        Radiology ->
            "Radiology Records"

        Hospitalizations ->
            "Hospitalization Records"

        Legal ->
            "Legal Records"

        CallRecordings ->
            "Call Recording Records"

        PreviousHistories ->
            "Previous History Records"

        Enrollment ->
            "Enrollment Records"

        Misc ->
            "Miscellaneous Records"


getRecordTypeById : Maybe Int -> Maybe RecordType
getRecordTypeById maybeInt =
    case maybeInt of
        Just int ->
            case int of
                1 ->
                    Just PrimaryCare

                2 ->
                    Just Specialty

                3 ->
                    Just Labs

                4 ->
                    Just Radiology

                9 ->
                    Just Hospitalizations

                6 ->
                    Just Legal

                10 ->
                    Just CallRecordings

                11 ->
                    Just PreviousHistories

                12 ->
                    Just Enrollment

                5 ->
                    Just Misc

                _ ->
                    Nothing

        Nothing ->
            Nothing


getId : RecordType -> Int
getId recordType =
    case recordType of
        PrimaryCare ->
            1

        Specialty ->
            2

        Labs ->
            3

        Radiology ->
            4

        Hospitalizations ->
            9

        Legal ->
            6

        CallRecordings ->
            10

        PreviousHistories ->
            11

        Enrollment ->
            12

        Misc ->
            5
