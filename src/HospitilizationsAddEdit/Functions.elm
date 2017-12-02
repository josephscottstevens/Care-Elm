module HospitilizationsAddEdit.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import HospitilizationsAddEdit.Types exposing (..)
import Common.Functions exposing (..)
import Common.Types exposing (AddEditDataSource, RecordAddNewInitData)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "Id", maybeVal Encode.int <| newRecord.id )
        , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.facilityId )
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.hospitalServiceTypeId )
        , ( "ChiefComplaint", Encode.string <| newRecord.chiefComplaint )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.dischargeDiagnosisId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.dischargePhysicianId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.facilityId2 )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge2 )
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


getHospitilizationMsg : AddEditDataSource -> Maybe Int -> Maybe RecordAddNewInitData -> InitHospitilizationsAddNew
getHospitilizationMsg addEditDataSource hospitilizationId recordAddNewInitData =
    case recordAddNewInitData of
        Just t ->
            { facilityId = addEditDataSource.facilityId
            , facilities = addEditDataSource.facilities
            , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
            , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
            , hospitilizations = addEditDataSource.hospitilizations
            , hospitilizationId = hospitilizationId

            -- for edit
            , patientId = t.patientId
            , patientReported = t.patientReported
            , hospitalizationId = t.hospitalizationId
            , dateOfAdmission = t.dateOfAdmission
            , dateOfDischarge = t.dateOfDischarge
            , hospitalServiceTypeId = t.hospitalServiceTypeId
            , chiefComplaint = t.chiefComplaint
            , admitDiagnosisId = t.admitDiagnosisId
            , dischargeDiagnosisId = t.dischargeDiagnosisId
            , dischargeRecommendations = t.dischargeRecommendations
            , dischargePhysicianId = t.dischargePhysicianId
            , facilityId2 = t.facilityId2
            , dateOfAdmission2 = t.dateOfAdmission2
            , dateOfDischarge2 = t.dateOfDischarge2
            }

        Nothing ->
            { facilityId = addEditDataSource.facilityId
            , patientId = addEditDataSource.patientId
            , facilities = addEditDataSource.facilities
            , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
            , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
            , hospitilizations = addEditDataSource.hospitilizations
            , hospitilizationId = hospitilizationId

            -- for edit
            , patientReported = False
            , hospitalizationId = Nothing
            , dateOfAdmission = Nothing
            , dateOfDischarge = Nothing
            , hospitalServiceTypeId = Nothing
            , chiefComplaint = ""
            , admitDiagnosisId = Nothing
            , dischargeDiagnosisId = Nothing
            , dischargeRecommendations = ""
            , dischargePhysicianId = Nothing
            , facilityId2 = Nothing
            , dateOfAdmission2 = Nothing
            , dateOfDischarge2 = Nothing
            }


updateModel : RecordAddNewInitData -> Model -> Model
updateModel t model =
    { model
        | patientId = t.patientId
        , facilityId = t.facilityId
        , patientReported = t.patientReported
        , hospitalizationId = t.hospitalizationId
        , dateOfAdmission = t.dateOfAdmission
        , dateOfDischarge = t.dateOfDischarge
        , hospitalServiceTypeId = t.hospitalServiceTypeId
        , chiefComplaint = t.chiefComplaint
        , admitDiagnosisId = t.admitDiagnosisId
        , dischargeDiagnosisId = t.dischargeDiagnosisId
        , dischargeRecommendations = t.dischargeRecommendations -- dischargeRecommendations?
        , dischargePhysicianId = t.dischargePhysicianId -- dischargePhysicianId?
        , facilityId2 = t.facilityId2 -- facilityId2?
        , dateOfAdmission2 = t.dateOfAdmission2
        , dateOfDischarge2 = t.dateOfDischarge2
    }
