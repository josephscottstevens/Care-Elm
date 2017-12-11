module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Functions exposing (..)
import Common.Types exposing (AddEditDataSource, HospitilizationsRow, HospitilizationsInitData)


encodeRecord : Model -> Int -> Encode.Value
encodeRecord newRecord patientId =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.initData.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.initData.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.initData.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.initData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.initData.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.initData.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.initData.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfDischarge2 )
        ]


saveFormRequest : Model -> Int -> Http.Request String
saveFormRequest model patientId =
    Http.request
        { body = encodeRecord model patientId |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewHospitilization"
        , withCredentials = False
        }


saveForm : Model -> Int -> Cmd Msg
saveForm model patientId =
    Http.send SaveCompleted (saveFormRequest model patientId)


getHospitilizationsInitData : AddEditDataSource -> Maybe HospitilizationsRow -> HospitilizationsInitData
getHospitilizationsInitData addEditDataSource maybeHospitilizationsRow =
    let
        hospitilizationsRow =
            Maybe.withDefault emptyHospitilizationRow maybeHospitilizationsRow
    in
        { id = Just hospitilizationsRow.id
        , facilities = addEditDataSource.facilities
        , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
        , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
        , facilityId = addEditDataSource.facilityId
        , admitDiagnosisId = hospitilizationsRow.admitDiagnosisId
        , dischargeDiagnosisId = hospitilizationsRow.dischargeDiagnosisId
        , facilityId2 = hospitilizationsRow.facilityId2
        , hospitalServiceTypeId = hospitilizationsRow.hospitalServiceTypeId
        , dischargePhysicianId = hospitilizationsRow.dischargePhysicianId
        , dateOfAdmission = hospitilizationsRow.dateOfAdmission
        , dateOfDischarge = hospitilizationsRow.dateOfDischarge
        , dateOfAdmission2 = hospitilizationsRow.dateOfAdmission2
        , dateOfDischarge2 = hospitilizationsRow.dateOfDischarge2
        }


updateModel : Model -> Maybe HospitilizationsRow -> Model
updateModel model maybeHospitilizationsRow =
    case maybeHospitilizationsRow of
        Just hospitilizationsRow ->
            { model
                | patientReported = hospitilizationsRow.patientReported
                , chiefComplaint = hospitilizationsRow.chiefComplaint
                , dischargeRecommendations = hospitilizationsRow.dischargeRecommendations
            }

        Nothing ->
            model
