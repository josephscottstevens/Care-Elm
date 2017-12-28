module HospitilizationsAddEdit.Functions exposing (getHospitilizationsInitData, saveForm)

import Json.Encode as Encode
import Http
import HospitilizationsAddEdit.Types exposing (Model, SyncfusionData, emptyHospitilizationRow)
import Common.Functions exposing (maybeVal, maybeToDateString)
import Common.Types exposing (AddEditDataSource, HospitilizationsRow)


encodeRecord : Model -> Int -> Encode.Value
encodeRecord newRecord patientId =
    Encode.object
        [ ( "Id", Encode.int <| newRecord.sfData.id )
        , ( "PatientId", Encode.int <| patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.sfData.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.sfData.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.sfData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.sfData.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.sfData.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.sfData.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.sfData.dateOfDischarge2 )
        ]


saveFormRequest : Model -> Int -> Http.Request String
saveFormRequest model patientId =
    Http.request
        { body = encodeRecord model patientId |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddEditHospitilization"
        , withCredentials = False
        }


saveForm : Model -> Int -> (Result Http.Error String -> msg) -> Cmd msg
saveForm model patientId saveCompleted =
    Http.send saveCompleted (saveFormRequest model patientId)


getHospitilizationsInitData : AddEditDataSource -> Maybe HospitilizationsRow -> SyncfusionData
getHospitilizationsInitData addEditDataSource maybeHospitilizationsRow =
    let
        hospitilizationsRow =
            Maybe.withDefault emptyHospitilizationRow maybeHospitilizationsRow
    in
        { id = hospitilizationsRow.id
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
