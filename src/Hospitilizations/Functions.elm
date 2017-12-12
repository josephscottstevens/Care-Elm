module Hospitilizations.Functions exposing (getHospitilizations, getLoadedState, flipDropDownOpen, deleteHospitilization, filterFields, filteredRecords)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Http
import Hospitilizations.Types exposing (Model, Filters)
import Common.Types exposing (FilterState, HospitilizationsRow)
import Common.Functions exposing (defaultLower, defaultDate)


-- Http helper functions


decodeHospitilizationsRow : Decode.Decoder HospitilizationsRow
decodeHospitilizationsRow =
    Pipeline.decode HospitilizationsRow
        |> Pipeline.required "Id" Decode.int
        |> Pipeline.required "FacilityName" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfAdmission" (Decode.maybe Decode.string)
        |> Pipeline.required "AdmitProblem" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDischarge" (Decode.maybe Decode.string)
        |> Pipeline.required "DischargeProblem" (Decode.maybe Decode.string)
        |> Pipeline.required "ServiceType" (Decode.maybe Decode.string)
        |> Pipeline.required "FromTcm" Decode.bool
        |> Pipeline.required "RecordId" (Decode.maybe Decode.int)
        |> Pipeline.hardcoded False
        -- For edit only
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "FacilityId" (Decode.maybe Decode.int)
        |> Pipeline.required "PatientReported" Decode.bool
        |> Pipeline.required "HospitalServiceTypeId" (Decode.maybe Decode.int)
        |> Pipeline.required "ChiefComplaint" Decode.string
        |> Pipeline.required "AdmitDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeDiagnosisId" (Decode.maybe Decode.int)
        |> Pipeline.required "DischargeRecommendations" Decode.string
        |> Pipeline.required "DischargePhysicianId" (Decode.maybe Decode.int)
        |> Pipeline.required "FacilityId2" (Decode.maybe Decode.int)
        |> Pipeline.required "DateOfAdmission2" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfDischarge2" (Decode.maybe Decode.string)


request : Int -> Http.Request (List HospitilizationsRow)
request patientId =
    Decode.list decodeHospitilizationsRow
        |> Http.get ("/People/HospitilizationsGrid?patientId=" ++ toString patientId)


getHospitilizations : Int -> (Result Http.Error (List HospitilizationsRow) -> msg) -> Cmd msg
getHospitilizations patientId t =
    Http.send t (request patientId)


deleteHospitilization : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteHospitilization rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteHospitilization?id=" ++ toString rowId)


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


getLoadedState : Model -> List HospitilizationsRow -> Model
getLoadedState model hospitilizationsRow =
    { model | hospitilizations = hospitilizationsRow }



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
