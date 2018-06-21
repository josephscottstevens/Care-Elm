module Dialogs.CcmSummaryReports exposing (Msg, State, init, update, view)

--init

import Common.Dialog as Dialog
import Common.Dropdown as Dropdown exposing (defaultDropConfig)
import Common.Functions as Functions
import Common.Html exposing (InputControlType(CheckInput, ControlElement, HtmlElement, KnockInput), defaultConfig, dividerLabel, fullWidth, makeControls)
import Common.Types exposing (AddEditDataSource, DropdownItem, RequiredType(Optional, Required), monthDropdown, yearDropdown)
import Html exposing (Html, button, div, input, label, text)
import Html.Attributes exposing (attribute, checked, class, style, title, type_)
import Http
import Json.Encode as Encode


type alias State =
    { facilityId : Maybe Int
    , facilities : List DropdownItem
    , facilityDropState : Dropdown.DropState
    , sfData : SfData
    , saveToClientPortal : Bool
    , includeCcmOpenTasks : Bool
    , hideClinicalSummaryFields : Bool
    , saveToPreviousHistories : Bool
    , fileId : String
    , files : List DropdownItem
    , fileDropState : Dropdown.DropState
    }


view : Dialog.RootDialog -> Maybe State -> Html Msg
view rootDialog state =
    case state of
        Just t ->
            Dialog.view rootDialog
                { onConfirm = Confirmed
                , onCancel = Close
                , headerText = "Batch Individual"
                , dialogOptions = Dialog.simpleDialogOptions 800 500
                , dialogContent =
                    div [ class "form-horizontal", style [ ( "padding-left", "40px" ) ] ]
                        [ makeControls { controlAttributes = [ class "col-md-8" ] }
                            [ dividerLabel "Select Facility"
                            , ControlElement "Facility" <|
                                Dropdown.view rootDialog t.facilityDropState UpdateFacility t.facilities t.facilityId
                            , dividerLabel "Select Date Interval For Billable Patients"
                            , KnockInput "Start Date" Required "StartDate"
                            , KnockInput "End Date" Required "EndDate"
                            , CheckInput "Save to Client Portal" Optional t.saveToClientPortal UpdateSaveToClientPortal
                            , CheckInput "Include CCM Open Tasks" Optional t.includeCcmOpenTasks UpdateIncludeCcmOpenTasks
                            , CheckInput "Hide Clinical Summary Fields" Optional t.hideClinicalSummaryFields UpdateHideClinicalSummaryFields
                            , CheckInput "Save to Previous Histories" Optional t.saveToPreviousHistories UpdateSaveToPreviousHistories
                            , HtmlElement <|
                                div [ class "row" ] []
                            , if t.saveToClientPortal then
                                ControlElement "Facility" <|
                                    Dropdown.view rootDialog t.fileDropState UpdateFile t.files t.fileId
                              else
                                text ""
                            , HtmlElement <|
                                div [ class "row" ] []
                            , dividerLabel ""
                            ]
                        ]
                }

        Nothing ->
            text ""


type alias SfData =
    { startDate : Maybe String
    , endDate : Maybe String
    }


type Msg
    = Confirmed
    | UpdateFacility ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSfData SfData
    | UpdateFile ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSaveToClientPortal Bool
    | UpdateIncludeCcmOpenTasks Bool
    | UpdateHideClinicalSummaryFields Bool
    | UpdateSaveToPreviousHistories Bool
    | Close
    | RequestCompleted ( Int, Int, Int, Bool ) (Result Http.Error String)
    | ExcelCompleted (Result Http.Error String)


update : Msg -> State -> ( Maybe State, Cmd Msg )
update msg model =
    case msg of
        UpdateFacility ( newDropState, newId, newMsg ) ->
            ( Just { model | facilityDropState = newDropState, facilityId = newId }
            , newMsg
            )

        UpdateFile ( newDropState, newId, newMsg ) ->
            ( Just { model | monthDropState = newDropState, currentMonth = newId }
            , newMsg
            )

        UpdateSfData sfData ->
            ( Just { model | sfData = sfData }
            , Cmd.none
            )

        UpdateSaveToClientPortal t ->
            ( Just { model | saveToClientPortal = not t }
            , Cmd.none
            )

        UpdateIncludeCcmOpenTasks t ->
            ( Just { model | includeCcmOpenTasks = not t }
            , Cmd.none
            )

        UpdateHideClinicalSummaryFields t ->
            ( Just { model | hideClinicalSummaryFields = not t }
            , Cmd.none
            )

        UpdateSaveToPreviousHistories t ->
            ( Just { model | saveToPreviousHistories = not t }
            , Cmd.none
            )

        Close ->
            ( Nothing
            , Cmd.none
            )

        Confirmed ->
            case ( model.currentMonth, model.currentMonth, model.facilityId ) of
                ( Just month, Just year, Just facilityId ) ->
                    ( Nothing
                    , Functions.getStringRequestWithParams
                        "/CCM/ValidateCcmRateForInvoice"
                        [ ( "facilityId", toString facilityId )
                        , ( "month", toString month )
                        , ( "year", toString year )
                        , ( "saveToClientPortal", String.toLower <| toString model.saveToClientPortal )
                        , ( "includeOpenBillingRecords", "false" )
                        ]
                        |> Http.send (RequestCompleted ( facilityId, year, month, model.saveToClientPortal ))
                    )

                _ ->
                    ( Nothing
                    , Cmd.none
                    )

        RequestCompleted ( facilityId, year, month, saveToClientPortal ) requestResponse ->
            case requestResponse of
                Ok _ ->
                    ( Just model
                    , Functions.customPostRequest
                        [ ( "facilityId", Encode.int facilityId )
                        , ( "year", Encode.int year )
                        , ( "month", Encode.int month )
                        , ( "saveToClientPortal", Encode.bool saveToClientPortal )
                        , ( "includeOpenBillingRecords", Encode.bool False )
                        ]
                        "/CCM/GetInvoiceReportXls"
                        Http.expectString
                        |> Http.send ExcelCompleted
                    )

                Err t ->
                    ( Just model
                    , Functions.displayErrorMessage (toString t)
                    )

        ExcelCompleted requestResponse ->
            case requestResponse of
                Ok response ->
                    case Functions.getResponseProp response "fileName" of
                        Just fileName ->
                            ( Just model
                            , Cmd.batch
                                [ Functions.displaySuccessMessage "Invoice report created successfully."
                                , Functions.openFile ("/CCM/DownloadMonthlyInvoice?fileName=" ++ fileName)
                                ]
                            )

                        Nothing ->
                            ( Just model
                            , Functions.displayErrorMessage "Cannot download file right now, please try again later"
                            )

                Err t ->
                    ( Just model
                    , Functions.displayErrorMessage (toString t)
                    )


init : Maybe Int -> Maybe Int -> List DropdownItem -> State
init currentMonth currentYear facilities =
    { currentMonth = currentMonth
    , currentYear = currentYear
    , facilityId = Nothing
    , saveToClientPortal = False
    , facilities = facilities
    , facilityDropState = Dropdown.init { defaultDropConfig | domId = "facilityDropdown", showSearchText = True }
    , monthDropState = Dropdown.init { defaultDropConfig | domId = "monthDropdown" }
    , yearDropState = Dropdown.init { defaultDropConfig | domId = "yearDropdown" }
    }



-- Functions.getStringRequestWithParams
--     "/CommonForms/CustomRangeFilterViewPartial"
--     [ ( "billingProcess", "false" )
--     , ( "saveAsFileType", "zip" )
--     , ( "batchCcmIndividualReports", "true" )
--     ]
-- public class CCMCustomRangeFilterFormModel
-- {
--     public int ReportHcoId { get; set; }
--     public DateTime StartDate { get; set; }
--     public DateTime EndDate { get; set; }
--     public DateTime CurrentDate { get; set; }
--     public string SaveAsFileType { get; set; }
--     public bool BillingProcess { get; set; }
--     public bool BatchCcmIndividualReports { get; set; }
--     public bool BatchTcmI29DayReports { get; set; }
--     public bool IsTCM { get; set; }
--     public bool IncludeOpenBillingRecords { get; set; }
-- }
-- public ActionResult CustomRangeFilterViewPartial
--     ( bool billingProcess = false
--     , string saveAsFileType = null
--     , bool batchCcmIndividualReports = false
--     , bool batchTcmI29DayReports=false
--     , bool isTCM = false
--     , bool includeOpenBillingRecords = false
--     )
-- buttonSettings.ClientSideEvents.Click =
-- """function(s,e){
--     window.billingProcess = false;
--     window.saveAsFileType = 'zip';
--     window.batchCcmIndividualReports = true;
--     PopupCCMBatchMonthlyIndividualFilterPopup.Show();
--     }";"""
