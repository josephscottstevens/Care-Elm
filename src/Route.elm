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


routeToString : Route -> String
routeToString route =
    case route of
        None ->
            "#"

        Billing ->
            "#"

        Records _ ->
            ""

        RecordAddNew _ ->
            ""

        Hospitilizations ->
            ""

        HospitilizationsAdd ->
            "#/people/_hospitalizations/add"

        HospitilizationsEdit id ->
            "#/people/_hospitalizations/edit" ++ toString id

        Error t ->
            "#/Error" ++ t



--         "#/people/_primarycarerecords" ->
--     "#/people/_specialtyrecords" ->
--     "#/people/_labrecords" ->
--     "#/people/_radiologyrecords" ->
--     "#/people/_hospitalizationrecords" ->
--    "#/people/_legalrecords" ->
--     "#/people/_ccdrecords" ->
--     "#/people/_callrecordingrecords" ->
--     "#/people/_previoushistoryrecords" ->
--     "#/people/_enrollmentrecords" ->
--     "#/people/_miscrecords" ->


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map None (s "")
        , Url.map Billing (s "login")
        , Url.map (Records Specialty) (s "logout")
        , Url.map (RecordAddNew Specialty) (s "logout")
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
