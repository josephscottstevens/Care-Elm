module Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import RecordAddNew.Types exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)
import String exposing (toLower)


decodeDropDownItem : Decoder DropDownItem
decodeDropDownItem =
    decode DropDownItem
        |> required "Id" (maybe Decode.int)
        |> required "Name" Decode.string


decodeModel : Decoder AddEditDataSource
decodeModel =
    decode AddEditDataSource
        |> required "facilityId" (maybe Decode.int)
        |> required "facilityDropdown" (Decode.list decodeDropDownItem)
        |> required "recordTypeDropdown" (Decode.list decodeDropDownItem)
        |> required "userDropDown" (Decode.list decodeDropDownItem)
        |> required "taskDropDown" (Decode.list decodeDropDownItem)
        |> required "hospitilizationServiceTypeDropdown" (Decode.list decodeDropDownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (Decode.list decodeDropDownItem)


request : Int -> Http.Request AddEditDataSource
request patientId =
    Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ toString patientId) decodeModel


getDropDowns : Int -> (Result Http.Error AddEditDataSource -> msg) -> Cmd msg
getDropDowns patientId t =
    Http.send t (request patientId)
