module Common.Routes exposing (..)

import Navigation
import Common.Types exposing (..)
import Char exposing (isDigit)
import Common.Functions exposing (defaultIntStr)


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

                "#/people/records/primarycare" ->
                    Records PrimaryCare

                "#/people/records/specialty" ->
                    Records Specialty

                "#/people/records/labs" ->
                    Records Labs

                "#/people/records/radiology" ->
                    Records Radiology

                "#/people/records/hospitalizations" ->
                    Records Hospitalizations

                "#/people/records/legal" ->
                    Records Legal

                "#/people/records/previoushistories" ->
                    Records PreviousHistories

                "#/people/records/callrecordings" ->
                    Records CallRecordings

                "#/people/records/enrollment" ->
                    Records Enrollment

                "#/people/records/misc" ->
                    Records Misc

                "#/people/records/primarycare/addedit" ->
                    RecordAddNew PrimaryCare

                "#/people/records/specialty/addedit" ->
                    RecordAddNew Specialty

                "#/people/records/labs/addedit" ->
                    RecordAddNew Labs

                "#/people/records/radiology/addedit" ->
                    RecordAddNew Radiology

                "#/people/records/hospitalizations/addedit" ->
                    RecordAddNew Hospitalizations

                "#/people/records/legal/addedit" ->
                    RecordAddNew Legal

                "#/people/records/previoushistories/addedit" ->
                    RecordAddNew PreviousHistories

                "#/people/records/callrecordings/addedit" ->
                    RecordAddNew CallRecordings

                "#/people/records/enrollment/addedit" ->
                    RecordAddNew Enrollment

                "#/people/records/misc/addedit" ->
                    RecordAddNew Misc

                _ ->
                    None
        else if String.contains "#/people/_hospitalizations/edit/" urlHash then
            HospitilizationsAddEdit (Just (defaultIntStr nums))
        else
            None


getPatientId : String -> Int
getPatientId urlSearch =
    String.filter isDigit urlSearch
        |> String.toInt
        |> Result.withDefault -17


navHospitilizations : Cmd msg
navHospitilizations =
    Navigation.modifyUrl "#/people/_hospitalizations"


navHospitilizationsAddEdit : Maybe Int -> Cmd msg
navHospitilizationsAddEdit hospitalizationId =
    case hospitalizationId of
        Just t ->
            Navigation.modifyUrl ("#/people/_hospitalizations/edit/" ++ (toString t))

        Nothing ->
            Navigation.modifyUrl "#/people/_hospitalizations/add"


navRecords : RecordType -> Cmd msg
navRecords recordType =
    let
        record =
            String.toLower (toString recordType)
    in
        Navigation.modifyUrl ("#/people/records/" ++ record)


navRecordAddNew : RecordType -> Cmd msg
navRecordAddNew recordType =
    let
        record =
            String.toLower (toString recordType)
    in
        Navigation.modifyUrl ("#/people/records/" ++ record ++ "/addedit")
