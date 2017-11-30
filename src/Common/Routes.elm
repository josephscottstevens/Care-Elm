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

        "#/people/_specialtyrecords" ->
            Records Specialty

        "#/people/_labrecords" ->
            Records Labs

        "#/people/_radiologyrecords" ->
            Records Radiology

        "#/people/_hospitalizationrecords" ->
            Records Hospitalizations

        "#/people/_legalrecords" ->
            Records Legal

        "#/people/_previoushistoryrecords" ->
            Records PreviousHistories

        "#/people/_callrecordingrecords" ->
            Records CallRecordings

        "#/people/_enrollmentrecords" ->
            Records Enrollment

        "#/people/_miscrecords" ->
            Records Misc

        "#/people/_records/addedit/PrimaryCare" ->
            RecordAddNew PrimaryCare

        "#/people/_records/addedit/Specialty" ->
            RecordAddNew Specialty

        "#/people/_records/addedit/Labs" ->
            RecordAddNew Labs

        "#/people/_records/addedit/Radiology" ->
            RecordAddNew Radiology

        "#/people/_records/addedit/Hospitalizations" ->
            RecordAddNew Hospitalizations

        "#/people/_records/addedit/Legal" ->
            RecordAddNew Legal

        "#/people/_records/addedit/PreviousHistories" ->
            RecordAddNew PreviousHistories

        "#/people/_records/addedit/CallRecordings" ->
            RecordAddNew CallRecordings

        "#/people/_records/addedit/Enrollment" ->
            RecordAddNew Enrollment

        "#/people/_records/addedit/Misc" ->
            RecordAddNew Misc

        _ ->
            Error (locationHref ++ " not found")


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


navRecords : Cmd msg
navRecords =
    Navigation.modifyUrl "#/people/_records"


navRecordAddNew : RecordType -> Cmd msg
navRecordAddNew recordType =
    Navigation.modifyUrl ("#/people/_records/addedit/" ++ (toString recordType))
