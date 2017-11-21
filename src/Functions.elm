module Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import Utils.CommonTypes exposing (..)


decodeDropDownItem : Decoder DropDownItem
decodeDropDownItem =
    decode DropDownItem
        |> required "Id" (maybe Decode.int)
        |> required "Name" Decode.string


decodeModel : Maybe Int -> Decoder AddEditDataSource
decodeModel recordTypeId =
    decode AddEditDataSource
        |> required "facilityId" (maybe Decode.int)
        |> required "facilityDropdown" (Decode.list decodeDropDownItem)
        |> required "recordTypeDropdown" (Decode.list decodeDropDownItem)
        |> required "userDropDown" (Decode.list decodeDropDownItem)
        |> required "taskDropDown" (Decode.list decodeDropDownItem)
        |> required "hospitilizationServiceTypeDropdown" (Decode.list decodeDropDownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (Decode.list decodeDropDownItem)
        |> hardcoded recordTypeId


request : Maybe Int -> Int -> Http.Request AddEditDataSource
request recordTypeId patientId =
    Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ toString patientId) (decodeModel recordTypeId)


getDropDowns : Maybe Int -> Int -> (Result Http.Error AddEditDataSource -> msg) -> Cmd msg
getDropDowns recordTypeId patientId t =
    Http.send t (request recordTypeId patientId)
