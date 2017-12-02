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
        |> required "RecordId" (maybe Decode.int)
        |> hardcoded False
        -- For edit only
        |> required "PatientId" Decode.int
        |> required "FacilityId" (maybe Decode.int)
        |> required "PatientReported" Decode.bool
        |> required "HospitalizationId" (maybe Decode.int)
        |> required "HospitalServiceTypeId" (maybe Decode.int)
        |> required "ChiefComplaint" Decode.string
        |> required "AdmitDiagnosisId" (maybe Decode.int)
        |> required "DischargeDiagnosisId" (maybe Decode.int)
        |> required "DischargeRecommendations" Decode.string
        |> required "DischargePhysicianId" (maybe Decode.int)
        |> required "FacilityId2" (maybe Decode.int)
        |> required "DateOfAdmission2" (maybe Decode.string)
        |> required "DateOfDischarge2" (maybe Decode.string)


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


deleteHospitilization : Int -> Cmd Msg
deleteHospitilization rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)



-- update helper functions
-- getRecordId : Model -> Maybe Int
-- getRecordId model =
--     let
--         hospitilizations =
--             model.hospitilizations
--                 |> List.filter (\t -> t.id == model.dropDownState.rowId)
--                 |> List.head
--                 |> Maybe.map (\t -> t.recordId)
--     in
--         Maybe.withDefault Nothing hospitilizations


flipDropDownOpen : List HospitilizationsRow -> Int -> List HospitilizationsRow
flipDropDownOpen hospitilizations recordId =
    hospitilizations
        |> List.map
            (\t ->
                if t.id == recordId then
                    { t | dropDownOpen = not t.dropDownOpen }
                else
                    { t | dropDownOpen = False }
            )


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
            { flds | recordId = t }
        else
            flds


filteredRecords : Model -> List HospitilizationsRow
filteredRecords model =
    model.hospitilizations
        |> List.filter (\t -> String.contains model.filterFields.id (toString t.id))
        |> List.filter (\t -> String.contains model.filterFields.facilityName (defaultLower t.facilityName))
        |> List.filter (\t -> String.contains model.filterFields.dateOfAdmission (defaultDate t.dateOfAdmission))
        |> List.filter (\t -> String.contains model.filterFields.admitProblem (defaultLower t.admitProblem))
        |> List.filter (\t -> String.contains model.filterFields.dateOfDischarge (defaultDate t.dateOfDischarge))
        |> List.filter (\t -> String.contains model.filterFields.dischargeProblem (defaultLower t.dischargeProblem))
        |> List.filter (\t -> String.contains model.filterFields.serviceType (defaultLower t.serviceType))
        |> List.filter (\t -> String.contains model.filterFields.fromTcm (toString t.fromTcm))
        |> List.filter (\t -> String.contains model.filterFields.recordId (toString t.recordId))
