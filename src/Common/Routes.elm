module Common.Routes exposing (..)

import Navigation
import Common.Types exposing (..)
import Char exposing (isDigit)


getPage : String -> Page
getPage locationHref =
    case locationHref of
        "#/people/_hospitalizations" ->
            Hospitilizations

        "#/people/_hospitalizations/addedit" ->
            HospitilizationsAddEdit

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


getPatientId : String -> Int
getPatientId locationHref =
    String.filter isDigit locationHref
        |> String.toInt
        |> Result.withDefault -17


navHospitilizations : Cmd msg
navHospitilizations =
    Navigation.modifyUrl "#/people/_hospitalizations"


navHospitilizationsAddEdit : Cmd msg
navHospitilizationsAddEdit =
    Navigation.modifyUrl "#/people/_hospitalizations/addedit"


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
