module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Functions exposing (..)
import Common.Types exposing (AddEditDataSource, HospitilizationsRow, HospitilizationsInitData)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.initData.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.initData.patientReported )

        -- , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.initData.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.initData.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.initData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.initData.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.initData.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.initData.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.initData.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.initData.dateOfDischarge2 )
        ]


saveFormRequest : Model -> Http.Request String
saveFormRequest model =
    Http.request
        { body = encodeRecord model |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewHospitilization"
        , withCredentials = False
        }


saveForm : Model -> Cmd Msg
saveForm model =
    Http.send SaveCompleted (saveFormRequest model)


getHospitilizationsInitData : AddEditDataSource -> Maybe HospitilizationsRow -> HospitilizationsInitData
getHospitilizationsInitData addEditDataSource maybeHospitilizationsRow =
    let
        hospitilizationsRow =
            case maybeHospitilizationsRow of
                Just t ->
                    t

                Nothing ->
                    emptyHospitilizationRow
    in
        { facilities = addEditDataSource.facilities
        , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
        , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
        , patientId = addEditDataSource.patientId
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

        -- Our control
        , patientReported = hospitilizationsRow.patientReported
        , chiefComplaint = hospitilizationsRow.chiefComplaint
        , dischargeRecommendations = hospitilizationsRow.dischargeRecommendations
        }
