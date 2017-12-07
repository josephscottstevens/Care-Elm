module Route exposing (Route(..), getPatientId, fromLocation, href, modifyUrl)

import Navigation
import Common.Types exposing (..)
import Char exposing (isDigit)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Regex exposing (..)
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, int, string, custom)


type Route
    = None
    | Billing
    | Records RecordType
    | RecordAddNew RecordType
    | Hospitilizations
    | HospitilizationsAdd
    | HospitilizationsEdit Int
    | Error String


formatRecordType : RecordType -> String
formatRecordType recordType =
    "_" ++ (toString recordType |> String.toLower) ++ "records/addedit"


routeToString : Route -> String
routeToString route =
    case route of
        None ->
            "#"

        Billing ->
            "#"

        Records _ ->
            ""

        RecordAddNew recordType ->
            "#/people/" ++ (formatRecordType recordType)

        Hospitilizations ->
            ""

        HospitilizationsAdd ->
            "#/people/_hospitalizations/add"

        HospitilizationsEdit id ->
            "#/people/_hospitalizations/edit" ++ toString id

        Error t ->
            "#/Error" ++ t


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map None (s "")
        , Url.map Billing (s "login")

        -- Records Grid
        , Url.map (Records PrimaryCare) (s "people" </> s "_primarycarerecords")
        , Url.map (Records Specialty) (s "people" </> s "_specialtyrecords")
        , Url.map (Records Labs) (s "people" </> s "_labrecords")
        , Url.map (Records Radiology) (s "people" </> s "_radiologyrecords")
        , Url.map (Records Hospitalizations) (s "people" </> s "_hospitalizationrecords")
        , Url.map (Records Legal) (s "people" </> s "_legalrecords")
        , Url.map (Records CallRecordings) (s "people" </> s "_callrecordingrecords")
        , Url.map (Records PreviousHistories) (s "people" </> s "_previoushistoryrecords")
        , Url.map (Records Enrollment) (s "people" </> s "_enrollmentrecords")
        , Url.map (Records Misc) (s "people" </> s "_miscrecords")

        -- Records Edit
        , Url.map (RecordAddNew PrimaryCare) (s "people" </> s "_primarycarerecords" </> s "addedit")
        , Url.map (RecordAddNew Specialty) (s "people" </> s "_specialtyrecords" </> s "addedit")
        , Url.map (RecordAddNew Labs) (s "people" </> s "_labrecords" </> s "addedit")
        , Url.map (RecordAddNew Radiology) (s "people" </> s "_radiologyrecords" </> s "addedit")
        , Url.map (RecordAddNew Hospitalizations) (s "people" </> s "_hospitalizationrecords" </> s "addedit")
        , Url.map (RecordAddNew Legal) (s "people" </> s "_legalrecords" </> s "addedit")
        , Url.map (RecordAddNew CallRecordings) (s "people" </> s "_callrecordingrecords" </> s "addedit")
        , Url.map (RecordAddNew PreviousHistories) (s "people" </> s "_previoushistoryrecords" </> s "addedit")
        , Url.map (RecordAddNew Enrollment) (s "people" </> s "_enrollmentrecords" </> s "addedit")
        , Url.map (RecordAddNew Misc) (s "people" </> s "_miscrecords" </> s "addedit")

        --
        , Url.map HospitilizationsAdd (s "settings")
        , Url.map HospitilizationsEdit (s "register" </> int)
        , Url.map Error (s "article" </> string)
        ]


getPatientId : String -> Maybe Int
getPatientId urlSearch =
    urlSearch
        |> find (AtMost 1) (regex "patientId=(\\d+)")
        |> List.map .match
        |> List.head
        |> Maybe.map (String.filter isDigit)
        |> Maybe.map String.toInt
        |> Maybe.andThen Result.toMaybe


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just None
    else
        parseHash route location
