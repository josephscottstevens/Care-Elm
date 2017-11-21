module RecordAddNew.Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import RecordAddNew.Types exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)
import String exposing (toLower)


encodeRecord : Model -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )

        -- , ( "PatientId", Encode.int <| newRecord.patientId )
        , ( "Title", Encode.string <| newRecord.title )

        -- , ( "RecordTypeId", maybeVal Encode.int <| newRecord.recordTypeId )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| newRecord.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| newRecord.timeAcc )
        , ( "RecordFile", Encode.string <| newRecord.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| newRecord.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| newRecord.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| newRecord.userId )
        , ( "TaskId", maybeVal Encode.int <| newRecord.taskId )

        -- Hospitilizations
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.dateOfDischarge )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeRecommendations )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.dischargePhysicianId )
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
