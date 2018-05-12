module Dialogs.InvoiceReportsDialog exposing (Msg, State, update, view)

--init

import Common.Dropdown as Dropdown
import Common.Functions as Functions
import Common.Html exposing (InputControlType(CheckInput, ControlElement, HtmlElement), makeControls)
import Common.Types exposing (DropdownItem, RequiredType(Optional), monthDropdown, yearDropdown)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Http
import Json.Encode as Encode


type alias State =
    { currentMonth : Maybe Int
    , currentYear : Maybe Int
    , facilityId : Maybe Int
    , saveToClientPortal : Bool
    , facilities : List DropdownItem
    , facilityDropState : Dropdown.DropState
    , monthDropState : Dropdown.DropState
    , yearDropState : Dropdown.DropState
    , showDialog : Bool
    }


view : Maybe State -> Html Msg
view state =
    case state of
        Just t ->
            div [ class "form-horizontal", style [ ( "padding-left", "40px" ) ] ]
                [ makeControls { controlAttributes = [ class "col-md-8" ] }
                    [ dividerLabel "Select Facility"
                    , ControlElement "Facility" <|
                        Dropdown.view t.facilityDropState UpdateFacility t.facilities t.facilityId
                    , dividerLabel "Select Month and Year For Billable Patients"
                    , ControlElement "Month" <|
                        Dropdown.view t.monthDropState UpdateMonth monthDropdown t.currentMonth
                    , ControlElement "Year" <|
                        Dropdown.view t.yearDropState UpdateYear yearDropdown t.currentYear
                    , CheckInput "Save to Client Portal" Optional t.saveToClientPortal UpdateSaveToClientPortal
                    , HtmlElement <|
                        div [ class "row" ] []
                    , dividerLabel ""
                    ]
                ]

        Nothing ->
            text ""


dividerLabel : String -> InputControlType msg
dividerLabel labelText =
    HtmlElement <|
        div
            [ style
                [ ( "border-bottom-style", "solid" )
                , ( "border-bottom-width", "1px" )
                , ( "border-bottom-color", "rgb(209, 209, 209)" )
                , ( "font-size", "14px" )
                , ( "color", "#808080" )
                , ( "padding", "3px" )
                , ( "font-weight", "400 !important" )
                , ( "width", "90%" )
                , ( "margin-top", "10px" )
                , ( "margin-bottom", "20px" )
                ]
            ]
            [ text labelText
            ]


type Msg
    = Confirmed State
    | UpdateFacility ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateMonth ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateYear ( Dropdown.DropState, Maybe Int, Cmd Msg )
    | UpdateSaveToClientPortal Bool
    | Close
    | RequestCompleted ( Int, Int, Int, Bool ) (Result Http.Error String)
    | ExcelCompleted (Result Http.Error String)


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        UpdateFacility ( newDropState, newId, newMsg ) ->
            { model | facilityDropState = newDropState, facilityId = newId }
                ! [ newMsg ]

        UpdateMonth ( newDropState, newId, newMsg ) ->
            { model | monthDropState = newDropState, currentMonth = newId }
                ! [ newMsg ]

        UpdateYear ( newDropState, newId, newMsg ) ->
            { model | yearDropState = newDropState, currentYear = newId }
                ! [ newMsg ]

        UpdateSaveToClientPortal t ->
            { model | saveToClientPortal = not t } ! []

        Close ->
            { model | showDialog = False } ! []

        Confirmed invoiceReportsDialog ->
            case ( model.currentMonth, model.currentMonth, model.facilityId ) of
                ( Just month, Just year, Just facilityId ) ->
                    { model | showDialog = False }
                        ! [ Functions.getStringRequestWithParams
                                "/CCM/ValidateCcmRateForInvoice"
                                [ ( "facilityId", toString facilityId )
                                , ( "month", toString month )
                                , ( "year", toString year )
                                , ( "saveToClientPortal", String.toLower <| toString model.saveToClientPortal )
                                , ( "includeOpenBillingRecords", "false" )
                                ]
                                |> Http.send (RequestCompleted ( facilityId, year, month, model.saveToClientPortal ))
                          ]

                _ ->
                    { model | showDialog = False } ! []

        RequestCompleted ( facilityId, year, month, saveToClientPortal ) requestResponse ->
            case requestResponse of
                Ok _ ->
                    model
                        ! [ Functions.postStringRequestWithObject
                                "/CCM/GetInvoiceReportXls"
                                [ ( "facilityId", Encode.int facilityId )
                                , ( "year", Encode.int year )
                                , ( "month", Encode.int month )
                                , ( "saveToClientPortal", Encode.bool saveToClientPortal )
                                , ( "includeOpenBillingRecords", Encode.bool False )
                                ]
                                |> Http.send ExcelCompleted
                          ]

                Err t ->
                    model ! [ Functions.displayErrorMessage (toString t) ]

        ExcelCompleted requestResponse ->
            case requestResponse of
                Ok response ->
                    case Functions.getResponseProp response "fileName" of
                        Just fileName ->
                            model
                                ! [ Functions.displaySuccessMessage "Invoice report created successfully."
                                  , Functions.openFile ("/CCM/DownloadMonthlyInvoice?fileName=" ++ fileName)
                                  ]

                        Nothing ->
                            model ! [ Functions.displayErrorMessage "Cannot download file right now, please try again later" ]

                Err t ->
                    model ! [ Functions.displayErrorMessage (toString t) ]



-- emptyInvoiceReportDialog : Maybe Int -> Maybe Int -> List DropdownItem -> State
-- emptyInvoiceReportDialog currentMonth currentYear facilities =
--     { currentMonth = currentMonth
--     , currentYear = currentYear
--     , facilityId = Nothing
--     , saveToClientPortal = False
--     , facilities = facilities
--     , facilityDropState = Dropdown.init "facilityDropdown" True
--     , monthDropState = Dropdown.init "monthDropdown" False
--     , yearDropState = Dropdown.init "yearDropdown" False
--     , showDialog = False
--     }
-- init : Maybe Int -> Maybe Int -> AddEditDataSource -> Dialog.Dialog State Msg
-- init currentMonth currentYear addEditDataSource =
--     { data = emptyInvoiceReportDialog currentMonth currentYear addEditDataSource.facilities
--     , headerText = "Invoice XLS"
--     , onConfirm = Confirmed
--     , onCancel = Close
--     , dialogContent = view
--     , dialogOptions = Dialog.simpleDialogOptions 800 500
--     }
