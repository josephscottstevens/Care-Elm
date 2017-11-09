module Records.Load exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Records.Model exposing (..)
import Utils.CommonTypes exposing (..)


decodeRecord : Decoder Record
decodeRecord =
    decode Record
        |> required "Id" (int)
        |> required "Date" (maybe string)
        |> required "Speciality" (maybe string)
        |> required "Comments" (maybe string)
        |> required "TransferedTo" (maybe string)
        |> required "TransferedOn" (maybe string)
        |> required "PatientId" (int)
        |> required "Title" (maybe string)
        |> required "DateAccessed" (maybe string)
        |> required "Provider" (maybe string)
        |> required "PatientName" (maybe string)
        |> required "RecordType" (maybe string)
        |> required "DateOfAdmission" (maybe string)
        |> required "DateOfDischarge" (maybe string)
        |> required "DischargePhysician" (maybe string)
        |> required "DischargeDiagnosis" (maybe string)
        |> required "HospitalizationServiceType" (maybe string)
        |> required "HospitalizationId" (maybe int)
        |> required "ReportDate" (maybe string)
        |> required "FileName" (maybe string)
        |> required "CanTransfer" (bool)
        |> required "Facility" (maybe string)
        |> required "FacilityFax" (maybe string)
        |> required "Recommendations" (maybe string)


decodeDropDownItem : Decoder DropDownItem
decodeDropDownItem =
    decode DropDownItem
        |> required "Id" (maybe int)
        |> required "Name" string


decodeModel : Decoder WebResponse
decodeModel =
    decode WebResponse
        |> required "list" (list decodeRecord)
        |> required "facilityDropdown" (list decodeDropDownItem)
        |> required "recordTypeDropdown" (list decodeDropDownItem)


request : Int -> Int -> Http.Request WebResponse
request patientId recordTypeId =
    Http.get ("/People/PatientRecordsGrid?patientId=" ++ (toString patientId) ++ "&recordTypeId=" ++ (toString recordTypeId)) decodeModel


getRecords : Int -> Int -> (Result Http.Error WebResponse -> msg) -> Cmd msg
getRecords patientId recordTypeId t =
    Http.send t (request patientId recordTypeId)


deleteRequest : Int -> Cmd Msg
deleteRequest rowId =
    Http.send DeleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ (toString rowId))
