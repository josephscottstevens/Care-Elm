module Records.Load exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Records.Model exposing (..)
import Utils.CommonTypes exposing (..)


decodeRecord : Decoder RecordRow
decodeRecord =
    decode RecordRow
        |> required "Id" Decode.int
        |> required "Date" (maybe Decode.string)
        |> required "Speciality" (maybe Decode.string)
        |> required "Comments" (maybe Decode.string)
        |> required "TransferedTo" (maybe Decode.string)
        |> required "TransferedOn" (maybe Decode.string)
        |> required "PatientId" Decode.int
        |> required "Title" (maybe Decode.string)
        |> required "DateAccessed" (maybe Decode.string)
        |> required "Provider" (maybe Decode.string)
        |> required "PatientName" (maybe Decode.string)
        |> required "RecordType" (maybe Decode.string)
        |> required "DateOfAdmission" (maybe Decode.string)
        |> required "DateOfDischarge" (maybe Decode.string)
        |> required "DischargePhysician" (maybe Decode.string)
        |> required "DischargeDiagnosis" (maybe Decode.string)
        |> required "HospitalizationServiceType" (maybe Decode.string)
        |> required "HospitalizationId" (maybe Decode.int)
        |> required "ReportDate" (maybe Decode.string)
        |> required "FileName" (maybe Decode.string)
        |> required "CanTransfer" Decode.bool
        |> required "Facility" (maybe Decode.string)
        |> required "FacilityFax" (maybe Decode.string)
        |> required "Recommendations" (maybe Decode.string)


encodeRecord : NewRecord -> Encode.Value
encodeRecord newRecord =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )
        , ( "PatientID", Encode.int <| newRecord.patientId )
        , ( "FacilityID", maybeInt Encode.int <| newRecord.facilityId )
        , ( "Facility", Encode.string <| newRecord.facilityText )
        , ( "RecordType", Encode.string <| newRecord.recordTypeText )
        , ( "RecordTypeId", Encode.int <| newRecord.recordTypeId )
        , ( "TimeVisit", Encode.string <| newRecord.timeVisit )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "Speciality", Encode.string <| newRecord.speciality )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "RecordFile", Encode.string <| newRecord.recordFile )
        ]


decodeDropDownItem : Decoder DropDownItem
decodeDropDownItem =
    decode DropDownItem
        |> required "Id" (maybe Decode.int)
        |> required "Name" Decode.string


decodeModel : Decoder WebResponse
decodeModel =
    decode WebResponse
        |> required "facilityId" (maybe Decode.int)
        |> required "list" (Decode.list decodeRecord)
        |> required "facilityDropdown" (Decode.list decodeDropDownItem)
        |> required "recordTypeDropdown" (Decode.list decodeDropDownItem)


request : Int -> Int -> Http.Request WebResponse
request patientId recordTypeId =
    Http.get ("/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId) decodeModel


getRecords : Int -> Int -> (Result Http.Error WebResponse -> msg) -> Cmd msg
getRecords patientId recordTypeId t =
    Http.send t (request patientId recordTypeId)


deleteRequest : Int -> Cmd Msg
deleteRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)


saveFormRequest : NewRecord -> Http.Request String
saveFormRequest record =
    Http.request
        { body = encodeRecord record |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewRecord"
        , withCredentials = False
        }


saveForm : NewRecord -> Cmd Msg
saveForm newRecord =
    Http.send SaveCompleted (saveFormRequest newRecord)


maybeInt : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybeInt encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null
