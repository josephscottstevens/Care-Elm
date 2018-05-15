port module Common.Functions exposing (..)

-- exposing
--     ( CustomDialog
--     , customDialogShow
--     , date
--     , dateFromString
--     , dateTime
--     , dateTimeToString
--     , dateToString
--     , decodeDropdownItem
--     , defaultBool
--     , defaultDate
--     , defaultDateTime
--     , defaultInt
--     , defaultIntStr
--     , defaultIntToString
--     , defaultLower
--     , defaultLowerDate
--     , defaultLowerDateTime
--     , defaultMaybeMsg
--     , defaultString
--     , deleteDialogShow
--     , dialogConfirmed
--     , displayErrorMessage
--     , displaySuccessMessage
--     , formatDateTime
--     , getAt
--     , getDesc
--     , getId
--     , getMonthIndex
--     , getRecordTypeById
--     , getRequestCompleted
--     , getRequestWithParams
--     , getResponseError
--     , getResponseProp
--     , getStringRequestWithParams
--     , idAttr
--     , isAlpha
--     , isNumber
--     , maybeStringToInt
--     , maybeVal
--     , noHash
--     , openFile
--     , postRequest
--     , postRequestWithObject
--     , postStringRequestWithObject
--     , secondsToHHMMSS
--     , sendMenuMessage
--     , setLoadingStatus
--     , setUnsavedChanges
--     , ticksToSeconds
--     , uniqueBy
--     , updateRows
--     )
--TODO, remove me

import Char exposing (isLower, isUpper)
import Common.Types
    exposing
        ( AddEditDataSource
        , DropdownItem
        , MenuMessage
        , RecordType(..)
        )
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, int, maybe, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Set exposing (Set)
import Time exposing (Month, Posix, Zone)



-- import Common.Route as Route exposing (Route)


port setLoadingStatus : Bool -> Cmd msg


port displaySuccessMessage : String -> Cmd msg


port displayErrorMessage : String -> Cmd msg


port sendMenuMessage : MenuMessage -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


type alias CustomDialog =
    { message : String
    , submitText : String
    , title : String
    , id : Int
    }


port customDialogShow : CustomDialog -> Cmd msg


port customDialogConfirmed : (CustomDialog -> msg) -> Sub msg


port openFile : String -> Cmd msg


displayError : Http.Error -> Cmd msg
displayError httpError =
    displayErrorMessage (getError httpError)


dialogConfirmed : (Int -> msg) -> Sub msg
dialogConfirmed msg =
    customDialogConfirmed (\t -> msg t.id)


deleteDialogShow : Int -> Cmd msg
deleteDialogShow id =
    customDialogShow
        { message = "Are you sure you wish to delete this record?"
        , submitText = "Delete"
        , title = "Confirm Delete"
        , id = id
        }


updateRows : List { b | id : a } -> { b | id : a } -> List { b | id : a }
updateRows rows newRow =
    List.map
        (\t ->
            if t.id == newRow.id then
                newRow

            else
                t
        )
        rows


isAlpha : Char -> Bool
isAlpha char =
    isLower char || isUpper char


isNumber : Char -> Bool
isNumber char =
    Char.isDigit char


idAttr : String -> String
idAttr str =
    String.filter isAlpha str ++ "Id"


noHash : Char -> Bool
noHash char =
    char == '#'


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
            String.fromInt t

        Nothing ->
            ""



-- formatDateTime : String -> Maybe String -> Maybe String
-- formatDateTime format maybeStr =
--     Debug.todo "TODO"
-- case maybeStr of
--     Just str ->
--         case Date.fromString str of
--             Ok date ->
--                 Just (Common.Date.toUtcFormattedString format date)
--             Err _ ->
--                 Nothing
--     Nothing ->
--         Nothing
-- dateFromString : String -> Maybe Date.Date
-- dateFromString str =
--     case Date.fromString str of
--         Ok t ->
--             Just t
--         Err _ ->
--             Nothing
-- dateTime : String -> String
-- dateTime str =
--     case Date.fromString str of
--         Ok date ->
--             dateTimeToString date
--         Err _ ->
--             ""
-- date : String -> String
-- date str =
--     case Date.fromString str of
--         Ok t ->
--             dateToString t
--         Err _ ->
--             ""
-- defaultDateTime : Maybe String -> String
-- defaultDateTime str =
--     dateTime (defaultString str)
-- defaultDate : Maybe String -> String
-- defaultDate str =
--     date (defaultString str)
-- defaultLowerDate : Maybe String -> String
-- defaultLowerDate str =
--     String.toLower (date (defaultString str))
-- defaultLowerDateTime : Maybe String -> String
-- defaultLowerDateTime str =
--     String.toLower (dateTime (defaultString str))
-- ticksToSeconds : Int -> Int
-- ticksToSeconds t =
--     t // 10000000
-- secondsToHHMMSS : Int -> String
-- secondsToHHMMSS t =
--     let
--         hours =
--             t // 3600
--         minutes =
--             (t - (hours * 3600)) // 60
--         seconds =
--             t - (hours * 3600) - (minutes * 60)
--         toSpacedString int =
--             if int < 10 then
--                 "0" ++ String.fromInt int
--             else
--                 String.fromInt int
--         pad str =
--             "[" ++ str ++ "]"
--     in
--     [ hours, minutes, seconds ]
--         |> List.map toSpacedString
--         |> List.foldr (\t y -> t ++ ":" ++ y) ""
--         |> String.dropRight 1
--         |> pad


maybeVal : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeVal encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null


getResponseProp : String -> String -> Maybe String
getResponseProp response propertyName =
    response
        |> Decode.decodeString (Decode.at [ "AdditionalData", propertyName ] Decode.string)
        |> Result.toMaybe


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
    Decode.succeed DropdownItem
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


postRequestWithObject : String -> List ( String, Encode.Value ) -> Decode.Decoder a -> Http.Request a
postRequestWithObject url object decoder =
    Http.request
        { body = Encode.object object |> Http.jsonBody
        , expect = Http.expectJson decoder
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = url
        , withCredentials = False
        }


postStringRequestWithObject : String -> List ( String, Encode.Value ) -> Http.Request String
postStringRequestWithObject url object =
    Http.request
        { body = Encode.object object |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = url
        , withCredentials = False
        }


getRequestWithParams : String -> List ( String, String ) -> Decoder a -> Http.Request a
getRequestWithParams url params =
    let
        urlParam =
            params
                |> List.map (\( t, y ) -> t ++ "=" ++ y)
                |> List.foldr (\t y -> t ++ "&" ++ y) ""
    in
    Http.get <|
        if urlParam == "" then
            url

        else
            url ++ "?" ++ urlParam


getStringRequestWithParams : String -> List ( String, String ) -> Http.Request String
getStringRequestWithParams url params =
    let
        urlParam =
            params
                |> List.map (\( t, y ) -> t ++ "=" ++ y)
                |> List.foldr (\t y -> t ++ "&" ++ y) ""
    in
    Http.getString <|
        if urlParam == "" then
            url

        else
            url ++ "?" ++ urlParam



-- getRequestCompleted : a -> Result b value -> ( a, Cmd msg )


getRequestCompleted model requestResponse =
    case requestResponse of
        Ok _ ->
            ( model, Cmd.none )

        Err err ->
            ( model, displayErrorMessage (getError err) )


getError : Http.Error -> String
getError err =
    case err of
        Http.BadUrl str ->
            "Error, Invalid URL: " ++ str

        Http.Timeout ->
            "Server is running slow, please try again later."

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus str ->
            -- case str of
            --     Http.Response (responseStr) ->
            "Bad Status: "

        -- ++ responseStr
        Http.BadPayload str1 str2 ->
            -- case str2 of
            -- Http.Response (responseStr) ->
            "Bad Payload: "



-- ++ str1 ++ " - " ++ responseStr
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


getAt : Int -> List a -> Maybe a
getAt idx xs =
    List.head <| List.drop idx xs
