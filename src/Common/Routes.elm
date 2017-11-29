module Common.Routes exposing (..)

import Navigation
import Common.Types as CommonTypes
import Char exposing (isDigit)


getPage : Navigation.Location -> CommonTypes.Page
getPage location =
    case location.href of
        "#/people/_hospitalizations/" ->
            CommonTypes.Hospitilizations

        "#/people/_hospitalizations/addedit" ->
            CommonTypes.HospitilizationsAddEdit

        _ ->
            CommonTypes.None


getRecordType : Navigation.Location -> CommonTypes.RecordType
getRecordType location =
    case location.href of
        "" ->
            CommonTypes.PrimaryCare

        _ ->
            CommonTypes.Misc


getPatientId : Navigation.Location -> Int
getPatientId location =
    String.filter isDigit location.search
        |> String.toInt
        |> Result.withDefault -17


navHospitilizations : Cmd msg
navHospitilizations =
    Navigation.load "#/people/_hospitalizations/"


navHospitilizationsAddEdit : Cmd msg
navHospitilizationsAddEdit =
    Navigation.load "#/people/_hospitalizations/addedit"


navRecords : Cmd msg
navRecords =
    Navigation.load "#/people/_records"


navRecordAddNew : Cmd msg
navRecordAddNew =
    Navigation.load "#/people/_records/addedit"
