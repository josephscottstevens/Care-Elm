module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, input, program, button, select, option, span, a)
import Html.Attributes exposing (style, class, placeholder, id, type_, value, tabindex)
import Html.Events exposing (onClick, onInput)
import Common.ServerTable as Table exposing (stringColumn, dateColumn)
import Common.Functions as Functions exposing (maybeVal, defaultString)
import Common.Types exposing (AddEditDataSource)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


init : Int -> Cmd Msg
init patientId =
    load patientId Table.defaultGridOperations


subscriptions : Sub msg
subscriptions =
    Sub.none


type alias Model =
    { rows : List Row
    , tableState : Table.State
    , query : String
    }


type alias Row =
    { id : Int
    , facility : Maybe String
    , facilityId : Int
    , practiceLocation : Maybe String
    , mainProvider : Maybe String
    , providerId : Int
    , patientName : Maybe String
    , patientId : Int
    , dob : Maybe String
    , patientFacilityIdNo : Maybe String
    , phone : Maybe String
    , assignedTo : Maybe String
    , staffId : Maybe Int
    , openTasks : Int
    , totalTimeSpent : Maybe Int
    , ccmRegistrationDate : Maybe String
    , dateOfService : Maybe String
    , billingDate : Maybe String
    , billingMonth : Int
    , billingYear : Int
    , isClosed : Bool
    , tocId : Maybe Int
    , readmission : Bool
    , isComplexCCM : Bool
    , batchCloseOnInvoiceCompletion : Bool
    , reviewedByStaffName : Maybe String
    , canModifyReviewedStatus : Bool
    , isReviewed : Bool
    , dxPresent : Bool
    , carePlanPresent : Bool
    , medsPresent : Bool
    , allergiesPresent : Bool
    , vitalsPresent : Bool
    , recordingPresent : Bool
    , chartComplete : Bool
    , status : Maybe String

    --, is24HoursSinceBilled : Bool
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    Table.view model.tableState model.rows (gridConfig addEditDataSource) Nothing


type Msg
    = Load (Result Http.Error LoadResult)
    | SetTableState Table.State
    | Reset


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok t) ->
            let
                tableState =
                    model.tableState
            in
                { model | rows = t.result, tableState = { tableState | totalRows = t.totalRows } } ! []

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetTableState newState ->
            { model | tableState = newState } ! [ load patientId (Table.getGridOperations newState) ]

        Reset ->
            model ! []



-- Paging stuff


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" (Decode.int)
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" (Decode.int)
        |> Pipeline.required "PracticeLocation" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Pipeline.required "ProviderId" (Decode.int)
        |> Pipeline.required "PatientName" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientId" (Decode.int)
        |> Pipeline.required "DoB" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "Phone" (Decode.maybe Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "StaffId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenTasks" (Decode.int)
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfService" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingDate" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingMonth" (Decode.int)
        |> Pipeline.required "BillingYear" (Decode.int)
        |> Pipeline.required "IsClosed" (Decode.bool)
        |> Pipeline.required "TocId" (Decode.maybe Decode.int)
        |> Pipeline.required "Readmission" (Decode.bool)
        |> Pipeline.required "IsComplexCCM" (Decode.bool)
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" (Decode.bool)
        |> Pipeline.required "ReviewedByStaffName" (Decode.maybe Decode.string)
        |> Pipeline.required "CanModifyReviewedStatus" (Decode.bool)
        |> Pipeline.required "IsReviewed" (Decode.bool)
        |> Pipeline.required "DxPresent" (Decode.bool)
        |> Pipeline.required "CarePlanPresent" (Decode.bool)
        |> Pipeline.required "MedsPresent" (Decode.bool)
        |> Pipeline.required "AllergiesPresent" (Decode.bool)
        |> Pipeline.required "VitalsPresent" (Decode.bool)
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" (Decode.bool)
        |> Pipeline.required "Status" (Decode.maybe Decode.string)



-- |> Pipeline.required "Is24HoursSinceBilled" (Decode.bool)


type alias LoadResult =
    { result : List Row
    , totalRows : Int
    }


jsonDecodeLoad : Decode.Decoder LoadResult
jsonDecodeLoad =
    Pipeline.decode LoadResult
        |> Pipeline.required "Data" (Decode.list decodeBillingCcm)
        |> Pipeline.required "Count" Decode.int


load : Int -> Table.GridOperations -> Cmd Msg
load patientId gridOperations =
    jsonDecodeLoad
        |> Functions.postJsonRequest (Table.encodeGridStuff (getColumns Nothing) gridOperations) ("/People/BillingTest?patientId=" ++ toString patientId)
        |> Http.send Load


emptyModel : Model
emptyModel =
    { rows = []
    , tableState = Table.init "Date"
    , query = ""
    }


gridConfig : Maybe AddEditDataSource -> Table.Config Row Msg
gridConfig addEditDataSource =
    { domTableId = "RecordTable"
    , toolbar = []

    -- case addEditDataSource of
    --     Just t ->
    --         [ ( "e-addnew", Add t recordType ) ]
    --     Nothing ->
    --         [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns = getColumns addEditDataSource
    }


getColumns : Maybe AddEditDataSource -> List (Table.Column Row Msg)
getColumns addEditDataSource =
    [ --checkColumn "" ,
      stringColumn "Facility" "Facility" .facility
    , stringColumn "BillingDate" "Billing Date" (\t -> Functions.dateFormat "MMMM YYYY" t.billingDate)
    , stringColumn "MainProvider" "Main Provider" .mainProvider
    , stringColumn "PatientName" "Patient Name" .patientName
    , dateColumn "DoB" "DOB" .dob
    , stringColumn "PatientFacilityIdNo" "Id No" .patientFacilityIdNo
    , stringColumn "AssignedTo" "AssignedTo" .assignedTo
    ]
