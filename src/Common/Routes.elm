module Common.Routes exposing (..)

import Navigation
import Common.Types exposing (..)
import Char exposing (isDigit)
import Common.Functions exposing (defaultIntStr)
import Regex exposing (..)


getPage : String -> Page
getPage urlHash =
    let
        nums =
            String.filter isDigit urlHash
    in
        if nums == "" then
            case urlHash of
                "#/people/_hospitalizations" ->
                    Hospitilizations

                "#/people/_hospitalizations/add" ->
                    HospitilizationsAddEdit Nothing

                "#/people/_primarycarerecords" ->
                    Records PrimaryCare

                "#/people/records/_primarycare" ->
                    Records PrimaryCare

                "#/people/records/_specialty" ->
                    Records Specialty

                "#/people/records/_labs" ->
                    Records Labs

                "#/people/records/_radiology" ->
                    Records Radiology

                "#/people/records/_hospitalizations" ->
                    Records Hospitalizations

                "#/people/records/_legal" ->
                    Records Legal

                "#/people/records/_previoushistories" ->
                    Records PreviousHistories

                "#/people/records/_callrecordings" ->
                    Records CallRecordings

                "#/people/records/_enrollment" ->
                    Records Enrollment

                "#/people/records/_misc" ->
                    Records Misc

                "#/people/records/_primarycare/addedit" ->
                    RecordAddNew PrimaryCare

                "#/people/records/_specialty/addedit" ->
                    RecordAddNew Specialty

                "#/people/records/_labs/addedit" ->
                    RecordAddNew Labs

                "#/people/records/_radiology/addedit" ->
                    RecordAddNew Radiology

                "#/people/records/_hospitalizations/addedit" ->
                    RecordAddNew Hospitalizations

                "#/people/records/_legal/addedit" ->
                    RecordAddNew Legal

                "#/people/records/_previoushistories/addedit" ->
                    RecordAddNew PreviousHistories

                "#/people/records/_callrecordings/addedit" ->
                    RecordAddNew CallRecordings

                "#/people/records/_enrollment/addedit" ->
                    RecordAddNew Enrollment

                "#/people/records/_misc/addedit" ->
                    RecordAddNew Misc

                _ ->
                    None
        else if String.contains "#/people/_hospitalizations/edit/" urlHash then
            HospitilizationsAddEdit (Just (defaultIntStr nums))
        else
            None


getPatientId : String -> Maybe Int
getPatientId urlSearch =
    urlSearch
        |> find (AtMost 1) (regex "patientId=(\\d+)")
        |> List.map .match
        |> List.head
        |> Maybe.map (String.filter isDigit)
        |> Maybe.map String.toInt
        |> Maybe.andThen Result.toMaybe


navHospitilizations : Cmd msg
navHospitilizations =
    Navigation.modifyUrl "#/people/_hospitalizations"


navHospitilizationsAddEdit : Maybe Int -> Cmd msg
navHospitilizationsAddEdit hospitalizationId =
    case hospitalizationId of
        Just t ->
            Navigation.modifyUrl ("#/people/_hospitalizations/edit/" ++ toString t)

        Nothing ->
            Navigation.modifyUrl "#/people/_hospitalizations/add"


navRecords : RecordType -> Cmd msg
navRecords recordType =
    let
        record =
            String.toLower (toString recordType)
    in
        Navigation.modifyUrl ("#/people/records/_" ++ record)


navRecordAddNew : RecordType -> Cmd msg
navRecordAddNew recordType =
    let
        record =
            String.toLower (toString recordType)
    in
        Navigation.modifyUrl ("#/people/records/_" ++ record ++ "/addedit")
