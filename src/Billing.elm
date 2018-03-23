module Billing exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html)
import Common.ServerTable as Table exposing (ColumnStyle(Width, CustomStyle), Operator(..))
import Common.Functions as Functions
import Common.Types exposing (AddEditDataSource)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


init : Int -> Cmd Msg
init patientId =
    Cmd.batch
        [ Table.initFilter columns
        , load patientId <| Table.init gridConfig
        ]


subscriptions : Sub Msg
subscriptions =
    Table.updateFilters UpdateFilters


type alias Model =
    { rows : List Row
    , gridOperations : Table.GridOperations Row Msg
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
    , is24HoursSinceBilled : Bool
    , billingCode : Maybe String
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model _ =
    Table.view model.gridOperations SetGridOperations model.rows Nothing


dxCpRcAlRxVsOperator : Operator
dxCpRcAlRxVsOperator =
    CustomSingleOperator "Equals" [ "DxPresent", "CarePlanPresent", "RecordingPresent", "AllergiesPresent", "MedsPresent", "VitalsPresent" ]


columns : List (Table.Column Row Msg)
columns =
    [ Table.htmlColumn "<= 24 Hrs" (Width 4) isNew Table.FilterIsNewControl (Contains "Is24HoursSinceBilled")
    , Table.checkColumn "Reviewed" (Width 4) .isReviewed (Table.Equals "IsReviewed")
    , Table.checkColumn "Batch Close" (Width 4) .batchCloseOnInvoiceCompletion (Equals "BatchCloseOnInvoiceCompletion")
    , Table.stringColumn "Facility" (Width 9) .facility (Table.Contains "Facility")
    , Table.htmlColumn "Billing Date" (Width 5) billingDate Table.Last60MonthsControl (Equals "BillingDate")
    , Table.stringColumn "Main Provider" (Width 5) .mainProvider (Contains "MainProvider")
    , Table.stringColumn "Patient Name" (Width 5) .patientName (Contains "PatientName")
    , Table.dateColumn "DOB" (Width 5) .dob (Contains "DoB")
    , Table.stringColumn "Patient's Facility Id" (Width 5) .patientFacilityIdNo (Contains "PatientFacilityIdNo")
    , Table.stringColumn "AssignedTo" (Width 5) .assignedTo (Contains "AssignedTo")
    , Table.stringColumn "Time Spent" (Width 4) timeSpent (Contains "TotalTimeSpent")
    , Table.hrefColumn "Open Tasks" (Width 3) openTasks (\_ -> Just "#/people/_tasks") (Equals "OpenTasks")
    , Table.dateColumn "CCM Enrollment" (Width 5) .ccmRegistrationDate (Contains "CcmRegistrationDate")
    , Table.stringColumn "Billing Codes" (Width 3) .billingCode (Contains "BillingCode")
    , Table.htmlColumn "Dx CP RC Al Rx VS" filterStyle dxCpRcAlRxVs Table.SixCirclesControl dxCpRcAlRxVsOperator
    , Table.dropdownColumn (Width 2)
        [ ( "", "Generate Summary Report", GenerateSummaryReport )
        , ( "", "Save Summary Report to Client Portal", SaveSummaryReportToClientPortal )
        , ( "", "Close Billing Session", CloseBillingSession )
        ]
    ]


isNew : Row -> Maybe String
isNew t =
    if t.is24HoursSinceBilled then
        Just "NEW"
    else
        Nothing


billingDate : Row -> Maybe String
billingDate t =
    Functions.formatDateTime "MMMM YYYY" t.billingDate


timeSpent : Row -> Maybe String
timeSpent t =
    t.totalTimeSpent
        |> Maybe.map Functions.ticksToSeconds
        |> Maybe.map Functions.secondsToHHMMSS


openTasks : Row -> Maybe String
openTasks t =
    Just (toString t.openTasks ++ " Tasks")


dxCpRcAlRxVs : Row -> Maybe String
dxCpRcAlRxVs t =
    -- let
    --     fullCircle =
    --         div [] []
    -- in
    -- [ t.dxPresent, t.carePlanPresent, t.recordingPresent, t.allergiesPresent, t.medsPresent, t.vitalsPresent ]
    --     |> List.map (\t -> if t then )
    Just """ <div class="circleMargin">
    <div title="Chronic Diagnoses present?" class="circle" style="background-color: currentColor"></div>
    <div title="Care Plan present?" class="circle" style="background-color: currentColor"></div>
    <div title="Recorded call present?" class="circle" style="background-color: white; border: 1.4px solid;"></div>
    <div title="Allergies present?" class="circle" style="background-color: currentColor"></div>
    <div title="Medication present?" class="circle" style="background-color: currentColor"></div>
    <div title="Vitals Signs present?" class="circle" style="background-color: currentColor"></div>
    </div>"""


filterStyle : ColumnStyle
filterStyle =
    CustomStyle
        [ ( "width", "100px" )
        , ( "padding-left", "10px" )
        ]


type Msg
    = Load (Result Http.Error LoadResult)
    | SetGridOperations (Table.GridOperations Row Msg)
    | UpdateFilters (List Table.Filter)
    | GenerateSummaryReport Row
    | SaveSummaryReportToClientPortal Row
    | CloseBillingSession Row


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    case msg of
        Load (Ok t) ->
            { model | rows = t.result, gridOperations = Table.updateFromServer t.serverData model.gridOperations }
                ! []

        Load (Err t) ->
            model ! [ Functions.displayErrorMessage (toString t) ]

        SetGridOperations gridOperations ->
            { model | gridOperations = gridOperations }
                ! [ load patientId gridOperations ]

        UpdateFilters filters ->
            let
                gridOperations =
                    Table.updateFilter filters model.gridOperations
            in
                { model | gridOperations = gridOperations }
                    ! [ load patientId gridOperations ]

        GenerateSummaryReport row ->
            Debug.crash "todo"

        SaveSummaryReportToClientPortal row ->
            Debug.crash "todo"

        CloseBillingSession row ->
            Debug.crash "todo"



-- Paging stuff


decodeBillingCcm : Decode.Decoder Row
decodeBillingCcm =
    Pipeline.decode Row
        |> Pipeline.required "ID" Decode.int
        |> Pipeline.required "Facility" (Decode.maybe Decode.string)
        |> Pipeline.required "FacilityId" Decode.int
        |> Pipeline.required "PracticeLocation" (Decode.maybe Decode.string)
        |> Pipeline.required "MainProvider" (Decode.maybe Decode.string)
        |> Pipeline.required "ProviderId" Decode.int
        |> Pipeline.required "PatientName" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientId" Decode.int
        |> Pipeline.required "DoB" (Decode.maybe Decode.string)
        |> Pipeline.required "PatientFacilityIdNo" (Decode.maybe Decode.string)
        |> Pipeline.required "Phone" (Decode.maybe Decode.string)
        |> Pipeline.required "AssignedTo" (Decode.maybe Decode.string)
        |> Pipeline.required "StaffId" (Decode.maybe Decode.int)
        |> Pipeline.required "OpenTasks" Decode.int
        |> Pipeline.required "TotalTimeSpent" (Decode.maybe Decode.int)
        |> Pipeline.required "CcmRegistrationDate" (Decode.maybe Decode.string)
        |> Pipeline.required "DateOfService" (Decode.maybe Decode.string)
        |> Pipeline.required "BillingDate" (Decode.maybe Decode.string)
        |> Pipeline.required "IsClosed" Decode.bool
        |> Pipeline.required "TocId" (Decode.maybe Decode.int)
        |> Pipeline.required "Readmission" Decode.bool
        |> Pipeline.required "IsComplexCCM" Decode.bool
        |> Pipeline.required "BatchCloseOnInvoiceCompletion" Decode.bool
        |> Pipeline.required "ReviewedByStaffName" (Decode.maybe Decode.string)
        |> Pipeline.required "CanModifyReviewedStatus" Decode.bool
        |> Pipeline.required "IsReviewed" Decode.bool
        |> Pipeline.required "DxPresent" Decode.bool
        |> Pipeline.required "CarePlanPresent" Decode.bool
        |> Pipeline.required "MedsPresent" Decode.bool
        |> Pipeline.required "AllergiesPresent" Decode.bool
        |> Pipeline.required "VitalsPresent" Decode.bool
        |> Pipeline.required "RecordingPresent" Decode.bool
        |> Pipeline.required "ChartComplete" Decode.bool
        |> Pipeline.required "Is24HoursSinceBilled" Decode.bool
        |> Pipeline.required "BillingCode" (Decode.maybe Decode.string)



-- |> Pipeline.required "Is24HoursSinceBilled" (Decode.bool)


type alias LoadResult =
    { result : List Row
    , serverData : Table.ServerData
    }


jsonDecodeLoad : Decode.Decoder LoadResult
jsonDecodeLoad =
    Pipeline.decode LoadResult
        |> Pipeline.required "Data" (Decode.list decodeBillingCcm)
        |> Pipeline.required "GridOperations" Table.decodeGridOperations


load : Int -> Table.GridOperations Row Msg -> Cmd Msg
load patientId gridOperations =
    Http.request
        { body =
            Encode.object
                [ ( "patientId", Encode.int patientId )
                , ( "gridOperations", Table.encodeGridOperations gridOperations )
                ]
                |> Http.jsonBody
        , expect = Http.expectJson jsonDecodeLoad
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/BillingTest"
        , withCredentials = False
        }
        |> Http.send Load


emptyModel : Model
emptyModel =
    { rows = []
    , gridOperations = Table.init gridConfig
    }


gridConfig : Table.Config Row Msg
gridConfig =
    { domTableId = "BillingTable"
    , sortField = Just "DOB"
    , toolbar = []
    , columns = columns
    }
