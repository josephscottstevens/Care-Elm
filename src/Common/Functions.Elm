port module Common.Functions
    exposing
        ( setLoadingStatus
        , displaySuccessMessage
        , displayErrorMessage
        , updateRows
        , idAttr
        , isAlpha
        , isNumber
        , noHash
        , defaultMaybeMsg
        , defaultString
        , defaultLower
        , defaultBool
        , defaultInt
        , defaultIntToString
        , formatDateTime
        , dateTimeToString
        , dateToString
        , dateFromString
        , dateTime
        , date
        , getMonthIndex
        , defaultDateTime
        , defaultDate
        , defaultLowerDate
        , defaultLowerDateTime
        , ticksToSeconds
        , secondsToHHMMSS
        , defaultIntStr
        , maybeVal
        , maybeStringToInt
        , getResponseError
        , decodeDropdownItem
        , postRequest
        , postRequestWithObject
        , postStringRequestWithObject
        , getRequestWithParams
        , getStringRequestWithParams
        , getRequestCompleted
        , getDesc
        , getRecordTypeById
        , getId
        , sendMenuMessage
        , setUnsavedChanges
        , CustomDialog
        , customDialogShow
        , deleteDialogShow
        , dialogConfirmed
        , openFile
        , uniqueBy
        , getAt
        )

import Char exposing (isLower, isUpper)
import Date
import Date.Extra
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, maybe, string, int, field, decodeString)
import Set exposing (Set)
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
            toString t

        Nothing ->
            ""


getMonthIndex : Date.Date -> Int
getMonthIndex dt =
    case Date.month dt of
        Date.Jan ->
            0

        Date.Feb ->
            1

        Date.Mar ->
            2

        Date.Apr ->
            3

        Date.May ->
            4

        Date.Jun ->
            5

        Date.Jul ->
            6

        Date.Aug ->
            7

        Date.Sep ->
            8

        Date.Oct ->
            9

        Date.Nov ->
            10

        Date.Dec ->
            11


formatDateTime : String -> Maybe String -> Maybe String
formatDateTime format maybeStr =
    case maybeStr of
        Just str ->
            case Date.fromString str of
                Ok date ->
                    Just (Date.Extra.toUtcFormattedString format date)

                Err _ ->
                    Nothing

        Nothing ->
            Nothing


dateTimeToString : Date.Date -> String
dateTimeToString date =
    Date.Extra.toUtcFormattedString "MM/dd/yyyy hh:mm:ss a" date


dateToString : Date.Date -> String
dateToString date =
    Date.Extra.toUtcFormattedString "MM/dd/yyyy" date


dateFromString : String -> Maybe Date.Date
dateFromString str =
    case Date.fromString str of
        Ok t ->
            Just t

        Err _ ->
            Nothing


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


ticksToSeconds : Int -> Int
ticksToSeconds t =
    t // 10000000


secondsToHHMMSS : Int -> String
secondsToHHMMSS t =
    let
        hours =
            t // 3600

        minutes =
            (t - (hours * 3600)) // 60

        seconds =
            t - (hours * 3600) - (minutes * 60)

        toSpacedString int =
            if int < 10 then
                "0" ++ toString int
            else
                toString int

        pad str =
            "[" ++ str ++ "]"
    in
        [ hours, minutes, seconds ]
            |> List.map toSpacedString
            |> List.foldr (\t y -> t ++ ":" ++ y) ""
            |> String.dropRight 1
            |> pad


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


getRequestCompleted : a -> Result b value -> ( a, Cmd msg )
getRequestCompleted model requestResponse =
    case requestResponse of
        Ok _ ->
            model ! []

        Err t ->
            model ! [ displayErrorMessage (toString t) ]



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
