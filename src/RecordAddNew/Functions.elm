module RecordAddNew.Functions exposing (..)

import Json.Encode as Encode exposing (..)
import Http
import RecordAddNew.Types exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", maybeVal Encode.int <| newRecord.recordTypeId )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.timeAcc )
        , ( "RecordFile", Encode.string <| newRecord.recordAddNewInitData.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.recordAddNewInitData.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| newRecord.recordAddNewInitData.userId )
        , ( "TaskId", maybeVal Encode.int <| newRecord.recordAddNewInitData.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.recordAddNewInitData.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.recordAddNewInitData.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.recordAddNewInitData.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.recordAddNewInitData.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.recordAddNewInitData.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.recordAddNewInitData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.recordAddNewInitData.dischargeDiagnosisId )
        ]


saveFormRequest : Model -> Http.Request String
saveFormRequest model =
    Http.request
        { body = encodeRecord model |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewRecord"
        , withCredentials = False
        }


saveForm : Model -> Cmd Msg
saveForm model =
    Http.send SaveCompleted (saveFormRequest model)
