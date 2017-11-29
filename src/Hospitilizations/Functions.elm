module Hospitilizations.Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Hospitilizations.Types exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)


-- Http helper functions


decodeRecordRow : Decoder HospitilizationsRow
decodeRecordRow =
    decode HospitilizationsRow
        |> required "Id" Decode.int
        |> required "FacilityName" (maybe Decode.string)
        |> required "DateOfAdmission" (maybe Decode.string)
        |> required "AdmitProblem" (maybe Decode.string)
        |> required "DateOfDischarge" (maybe Decode.string)
        |> required "DischargeProblem" (maybe Decode.string)
        |> required "ServiceType" (maybe Decode.string)
        |> required "FromTcm" Decode.bool
        |> required "HasRecord" Decode.bool


decodeModel : Decoder WebResponse
decodeModel =
    decode WebResponse
        |> required "list" (Decode.list decodeRecordRow)


request : Int -> Http.Request WebResponse
request patientId =
    Http.get ("/People/HospitilizationsGrid?patientId=" ++ toString patientId) decodeModel


getHospitilizations : Int -> (Result Http.Error WebResponse -> msg) -> Cmd msg
getHospitilizations patientId t =
    Http.send t (request patientId)


deleteRequest : Int -> Cmd Msg
deleteRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)



-- update helper functions


getLoadedState : Model -> WebResponse -> Model
getLoadedState model t =
    { model
        | state = Grid
        , hospitilizations = t.hospitilizations
    }



-- Filtering update helpers


filterFields : Filters -> FilterState -> Filters
filterFields flds filterState =
    let
        t =
            String.toLower filterState.value
    in
        if filterState.name == "Date Collected" then
            { flds | id = t }
        else if filterState.name == "Doctor of Visit" then
            { flds | facilityName = t }
        else if filterState.name == "Specialty" then
            { flds | dateOfAdmission = t }
        else if filterState.name == "Comments" then
            { flds | admitProblem = t }
        else if filterState.name == "Date Accessioned" then
            { flds | dateOfDischarge = t }
        else if filterState.name == "Name of Lab" then
            { flds | dischargeProblem = t }
        else if filterState.name == "Name of Study" then
            { flds | fromTcm = t }
        else if filterState.name == "Provider" then
            { flds | hasRecord = t }
        else
            flds


filteredRecords : Model -> List HospitilizationsRow
filteredRecords model =
    model.hospitilizations
        |> List.filter (\t -> String.contains model.filterFields.id (toString t.id))
        |> List.filter (\t -> String.contains model.filterFields.facilityName (defaultLower t.facilityName))
        |> List.filter (\t -> String.contains model.filterFields.dateOfAdmission (defaultDateTime t.dateOfAdmission))
        |> List.filter (\t -> String.contains model.filterFields.admitProblem (defaultDateTime t.admitProblem))
        |> List.filter (\t -> String.contains model.filterFields.dateOfDischarge (defaultDateTime t.dateOfDischarge))
        |> List.filter (\t -> String.contains model.filterFields.dischargeProblem (defaultLower t.dischargeProblem))
        |> List.filter (\t -> String.contains model.filterFields.serviceType (defaultLower t.serviceType))
        |> List.filter (\t -> String.contains model.filterFields.fromTcm (toString t.fromTcm))
        |> List.filter (\t -> String.contains model.filterFields.hasRecord (toString t.hasRecord))
