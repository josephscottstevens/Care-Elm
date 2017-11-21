module Functions exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Http
import RecordAddNew.Types exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)
import String exposing (toLower)


getAddEditDataSource : AddEditDataSource -> Model
getAddEditDataSource t =
    { model
        | facilityId = t.facilityId
        , facilities = t.facilities
        , recordTypes = t.recordTypes
        , tasks = t.tasks
        , users = t.users
        , hospitilizationServiceTypes = t.hospitilizationServiceTypes
        , hospitalizationDischargePhysicians = t.hospitalizationDischargePhysicians
    }


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
