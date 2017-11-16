port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)
import Dict exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


port toggleConsent : Bool -> Cmd msg


port editTask : Int -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port displaySuccessMessage : String -> Cmd msg


port displayErrorMessage : String -> Cmd msg


port setLoadingStatus : Bool -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateTimeVisit : (Maybe String -> msg) -> Sub msg


port updateTimeAcc : (Maybe String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port updateReportDate : (Maybe String -> msg) -> Sub msg


port updateRecordingDate : (Maybe String -> msg) -> Sub msg


port updateUser : (DropDownItem -> msg) -> Sub msg


port updateTask : (DropDownItem -> msg) -> Sub msg


port dropDownToggle : (Int -> msg) -> Sub msg


port deleteConfirmed : (Int -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        AddNew t ->
            Sub.batch
                [ updateFacility (UpdateFacility t)
                , updateCategory (UpdateRecordType t)
                , updateTimeVisit (UpdateTimeVisit t)
                , updateTimeAcc (UpdateTimeAcc t)
                , updateFileName (UpdateFileName t)
                , updateReportDate (UpdateReportDate t)
                , updateRecordingDate (UpdateRecordingDate t)
                , updateUser (UpdateUser t)
                , updateTask (UpdateTask t)
                ]

        _ ->
            Sub.batch
                [ dropDownToggle DropDownToggle
                , deleteConfirmed DeleteConfirmed
                ]


init : Flags -> Cmd Msg
init flag =
    case flag.recordType of
        Just recType ->
            getRecords flag.patientId recType Load

        Nothing ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Load (Ok t) ->
            getLoadedState model t ! [ setLoadingStatus False ]

        Load (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            { model | records = flipConsent model.records recordId model.recordTypeId } ! [ sendMenuMessage (getMenuMessage model recordId messageType) ]

        AddNewStart ->
            { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model) ]

        Save newRecord ->
            let
                actions =
                    if List.length (getValidationErrors (formInputs newRecord)) > 0 then
                        []
                    else
                        [ saveForm newRecord, setUnsavedChanges False ]
            in
                { model | state = AddNew { newRecord | showValidationErrors = True } } ! actions

        SaveCompleted (Ok responseMsg) ->
            case getResponseError responseMsg of
                Just t ->
                    model ! [ getRecords model.patientId model.recordTypeId Load, displayErrorMessage t ]

                Nothing ->
                    model ! [ getRecords model.patientId model.recordTypeId Load, displaySuccessMessage "Save completed successfully!" ]

        SaveCompleted (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        Cancel ->
            { model | state = Grid } ! [ setUnsavedChanges False ]

        DropDownToggle recordId ->
            { model | records = flipDropDownOpen model.records recordId } ! []

        DeleteConfirmed rowId ->
            let
                updatedRecords =
                    model.records |> List.filter (\t -> t.id /= rowId)
            in
                { model | records = updatedRecords } ! [ deleteRequest rowId ]

        DeleteCompleted (Ok responseMsg) ->
            case getResponseError responseMsg of
                Just t ->
                    model ! [ displayErrorMessage t ]

                Nothing ->
                    model ! [ displaySuccessMessage "Record deleted successfully!" ]

        DeleteCompleted (Err httpError) ->
            { model | state = Error (toString httpError) } ! []

        UpdateTitle newRecord str ->
            { model | state = AddNew { newRecord | title = str } } ! [ setUnsavedChanges True ]

        OpenTask taskId ->
            model ! [ editTask taskId ]

        UpdateRecordType newRecord dropDownItem ->
            case dropDownItem.id of
                Just t ->
                    { model | state = AddNew { newRecord | recordTypeId = t, recordTypeText = dropDownItem.name } } ! [ setUnsavedChanges True ]

                Nothing ->
                    model ! []

        SetFilter filterState ->
            { model | filterFields = filterFields model.filterFields filterState } ! []

        UpdateSpecialty newRecord str ->
            { model | state = AddNew { newRecord | specialty = str } } ! [ setUnsavedChanges True ]

        UpdateProvider newRecord str ->
            { model | state = AddNew { newRecord | provider = str } } ! [ setUnsavedChanges True ]

        UpdateTimeVisit newRecord str ->
            { model | state = AddNew { newRecord | timeVisit = str } } ! [ setUnsavedChanges True ]

        UpdateTimeAcc newRecord str ->
            { model | state = AddNew { newRecord | timeAcc = str } } ! [ setUnsavedChanges True ]

        UpdateFileName newRecord str ->
            { model | state = AddNew { newRecord | fileName = str } } ! [ setUnsavedChanges True ]

        UpdateComments newRecord str ->
            { model | state = AddNew { newRecord | comments = str } } ! [ setUnsavedChanges True ]

        UpdateFacility newRecord dropDownItem ->
            { model | state = AddNew { newRecord | facilityId = dropDownItem.id, facilityText = dropDownItem.name } } ! [ setUnsavedChanges True ]

        UpdateReportDate newRecord str ->
            { model | state = AddNew { newRecord | reportDate = str } } ! [ setUnsavedChanges True ]

        UpdateCallSid newRecord str ->
            { model | state = AddNew { newRecord | callSid = str } } ! [ setUnsavedChanges True ]

        UpdateRecordingSid newRecord str ->
            { model | state = AddNew { newRecord | recording = str } } ! [ setUnsavedChanges True ]

        UpdateDuration newRecord str ->
            { model | state = AddNew { newRecord | duration = defaultIntStr str } } ! [ setUnsavedChanges True ]

        UpdateRecordingDate newRecord str ->
            { model | state = AddNew { newRecord | recordingDate = str } } ! [ setUnsavedChanges True ]

        UpdateUser newRecord dropDownItem ->
            { model | state = AddNew { newRecord | userId = dropDownItem.id, userText = dropDownItem.name } } ! [ setUnsavedChanges True ]

        UpdateTask newRecord dropDownItem ->
            { model | state = AddNew { newRecord | taskId = dropDownItem.id, taskText = dropDownItem.name } } ! [ setUnsavedChanges True ]


view : Model -> Html Msg
view model =
    case model.state of
        Grid ->
            div []
                [ button [ type_ "button", class "btn btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config SetFilter model.recordTypeId (getTaskId model)) model.tableState (filteredRecords model) ]
                ]

        AddNew newRecord ->
            let
                inputControls =
                    makeControls (formInputs newRecord)

                errors =
                    getValidationErrors (formInputs newRecord)

                validationErrorsDiv =
                    if newRecord.showValidationErrors == True && List.length errors > 0 then
                        div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
                    else
                        div [] []

                saveBtnClass =
                    class "btn btn-success margin-left-5 pull-right"

                footerControls =
                    [ div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save newRecord), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]
            in
                div
                    [ class "form-horizontal" ]
                    (validationErrorsDiv :: inputControls ++ footerControls)

        Error errMessage ->
            div [] [ text errMessage ]


formInputs : NewRecord -> List ( String, RequiredType, InputControlType Msg )
formInputs newRecord =
    let
        recordType =
            getRecordType newRecord.recordTypeId

        defaultFields =
            [ ( "Date of Visit", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" (UpdateTimeVisit newRecord) )
            , ( "Doctor of Visit", Optional, TextInput newRecord.provider (UpdateProvider newRecord) )
            , ( "Specialty of Visit", Optional, TextInput newRecord.specialty (UpdateSpecialty newRecord) )
            , ( "Comments", Required, AreaInput newRecord.comments (UpdateComments newRecord) )
            , ( "Upload Record File", Required, FileInput newRecord.fileName )
            ]

        firstColumns =
            [ ( "Facility", Required, DropInput newRecord.facilityId "FacilityId" )
            , ( "Category", Required, DropInput (Just newRecord.recordTypeId) "CategoryId" )
            ]

        lastColumns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Specialty ->
                    defaultFields

                Labs ->
                    [ ( "Date/Time of Labs Collected", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" (UpdateTimeVisit newRecord) )
                    , ( "Date/Time of Labs Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" (UpdateTimeAcc newRecord) )
                    , ( "Name of Lab", Optional, TextInput newRecord.title (UpdateTitle newRecord) )
                    , ( "Provider of Lab", Optional, TextInput newRecord.provider (UpdateProvider newRecord) )
                    , ( "Comments", Required, AreaInput newRecord.comments (UpdateComments newRecord) )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Radiology ->
                    [ ( "Date/Time of Study was done", Required, DateInput (defaultString newRecord.timeVisit) "TimeVisitId" (UpdateTimeVisit newRecord) )
                    , ( "Date/Time of Study Accessioned", Required, DateInput (defaultString newRecord.timeAcc) "TimeAccId" (UpdateTimeAcc newRecord) )
                    , ( "Name of Study", Optional, TextInput newRecord.title (UpdateTitle newRecord) )
                    , ( "Provider of Study", Optional, TextInput newRecord.provider (UpdateProvider newRecord) )
                    , ( "Comments", Required, TextInput newRecord.comments (UpdateComments newRecord) )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Misc ->
                    defaultFields

                Legal ->
                    [ ( "Title", Optional, TextInput newRecord.title (UpdateTitle newRecord) )
                    , ( "Comments", Required, AreaInput newRecord.comments (UpdateComments newRecord) )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Hospitalizations ->
                    []

                CallRecordings ->
                    [ ( "Call Sid", Required, TextInput newRecord.callSid (UpdateCallSid newRecord) )
                    , ( "Recording Sid", Required, TextInput newRecord.recording (UpdateRecordingSid newRecord) )
                    , ( "Duration", Required, NumrInput newRecord.duration (UpdateDuration newRecord) )
                    , ( "Recording Date", Required, DateInput (defaultString newRecord.recordingDate) "RecordingDateId" (UpdateRecordingDate newRecord) )
                    , ( "User", Required, DropInput newRecord.userId "UserId" )
                    , ( "Task", Optional, DropInput newRecord.taskId "TaskId" )
                    ]

                PreviousHistories ->
                    [ ( "Report Date", Required, DateInput (defaultString newRecord.reportDate) "ReportDateId" (UpdateReportDate newRecord) )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]

                Enrollment ->
                    [ ( "Title", Optional, TextInput newRecord.title (UpdateTitle newRecord) )
                    , ( "Comments", Required, AreaInput newRecord.comments (UpdateComments newRecord) )
                    , ( "Upload Record File", Required, FileInput newRecord.fileName )
                    ]
    in
        List.append firstColumns lastColumns


getColumns : Int -> Maybe Int -> (data -> Int) -> List (Column RecordRow Msg)
getColumns recordTypeId taskId rowId =
    let
        commonColumns =
            [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
            , stringColumn "Specialty" (\t -> defaultString t.specialty)
            , stringColumn "Comments" (\t -> defaultString t.comments)
            ]

        firstColumns =
            case getRecordType recordTypeId of
                PrimaryCare ->
                    commonColumns

                Specialty ->
                    commonColumns

                Labs ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Lab" (\t -> defaultString t.title)
                    , stringColumn "Provider" (\t -> defaultString t.provider)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Radiology ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
                    , stringColumn "Name of Study" (\t -> defaultString t.title)
                    , stringColumn "Provider" (\t -> defaultString t.provider)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Hospitalizations ->
                    []

                Legal ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                CallRecordings ->
                    [ stringColumn "Date" (\t -> dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> defaultString t.recording)
                    , hrefColumnExtra "Task" (\t -> defaultString t.taskTitle) "#" (OpenTask (defaultInt taskId))
                    , checkColumn "During Enrollment" (\t -> t.enrollment)
                    , checkColumn "Consent" (\t -> t.hasVerbalConsent)
                    , stringColumn "User" (\t -> defaultString t.staffName)
                    ]

                PreviousHistories ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "File Name" (\t -> defaultString t.fileName)
                    , stringColumn "Report Date" (\t -> defaultDate t.reportDate)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Enrollment ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Misc ->
                    commonColumns

        lastColumns =
            [ rowDropDown recordTypeId
            ]
    in
        List.append firstColumns lastColumns


rowDropDown : Int -> Table.Column RecordRow Msg
rowDropDown recordTypeId =
    Table.veryCustomColumn
        { name = ""
        , viewData = (\t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordTypeId t.id))
        , sorter = Table.unsortable
        }


config : (FilterState -> Msg) -> Int -> Maybe Int -> Config RecordRow Msg
config msg recordTypeId taskId =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordTypeId taskId .id
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = (standardThead msg) }
        }


dropDownItems : Int -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordTypeId rowId =
    case getRecordType recordTypeId of
        CallRecordings ->
            [ ( "", "Mark As Consent", onClick (SendMenuMessage rowId "MarkAsConsent") ) ]

        _ ->
            [ ( "", "Transfer", onClick (SendMenuMessage rowId "Transfer") )
            , ( "e-contextedit", "View File", onClick (SendMenuMessage rowId "ViewFile") )
            , ( "", "Send By Email", onClick (SendMenuMessage rowId "SendByEmail") )
            , ( "", "Send By Fax", onClick (SendMenuMessage rowId "SendByFax") )
            , ( "", "Save To Client Portal", onClick (SendMenuMessage rowId "SaveToClientPortal") )
            , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "Delete") )
            ]
