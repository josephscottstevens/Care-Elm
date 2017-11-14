port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick)
import Table exposing (..)
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)


port sendMenuMessage : MenuMessage -> Cmd msg


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


port dropDownToggle : (DropDownState -> msg) -> Sub msg


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
            { model
                | state = Grid
                , facilityId = t.facilityId
                , records = t.records
                , facilities = t.facilities
                , recordTypes = t.recordTypes
                , tasks = t.tasks
                , users = t.users
            }
                ! [ setLoadingStatus False ]

        Load (Err httpError) ->
            { model | state = Error (toString httpError) } ! [ setLoadingStatus False ]

        SetTableState newState ->
            { model | tableState = newState } ! []

        SendMenuMessage recordId messageType ->
            model ! [ sendMenuMessage (MenuMessage messageType recordId model.recordTypeId) ]

        AddNewStart ->
            let
                newRecord =
                    { emptyNewRecord
                        | patientId = model.patientId
                        , recordTypeId = model.recordTypeId
                        , facilityId = model.facilityId
                    }
            in
                { model | state = AddNew newRecord }
                    ! [ initSyncfusionControls (SyncFusionMessage model.facilities model.recordTypes model.users model.tasks model.facilityId model.recordTypeId) ]

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

        DropDownToggle dropState ->
            { model | dropDownState = dropState } ! []

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

        UpdateRecordType newRecord dropDownItem ->
            case dropDownItem.id of
                Just t ->
                    { model | state = AddNew { newRecord | recordTypeId = t, recordTypeText = dropDownItem.name } } ! [ setUnsavedChanges True ]

                Nothing ->
                    model ! []

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
                , editDropDownDiv (dropDownItems model.recordTypeId model.dropDownState.rowId) model.dropDownState
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config model.recordTypeId) model.tableState model.records ]
                ]

        AddNew newRecord ->
            let
                inputControls =
                    makeControls (formInputs newRecord)

                errors =
                    getValidationErrors (formInputs newRecord)

                validationErrorsDiv =
                    if newRecord.showValidationErrors == True && List.length errors > 0 then
                        displayErrors errors
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


displayErrors : List String -> Html Msg
displayErrors errors =
    div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)


formInputs : NewRecord -> List (InputControlType Msg)
formInputs newRecord =
    let
        recordType =
            getRecordType newRecord.recordTypeId

        defaultFields =
            [ DropInput Required "Date of Visit" (defaultString newRecord.timeVisit) "TimeVisitId"
            , TextInput Optional "Doctor of Visit" newRecord.provider (UpdateProvider newRecord)
            , TextInput Optional "Specialty of Visit" newRecord.specialty (UpdateSpecialty newRecord)
            , AreaInput Required "Comments" newRecord.comments (UpdateComments newRecord)
            , FileInput Required "Upload Record File" newRecord.fileName
            ]

        firstColumns =
            [ DropInput Required "Facility" (defaultInt newRecord.facilityId) "FacilityId"
            , DropInput Required "Category" (toString newRecord.recordTypeId) "CategoryId"
            ]

        lastColumns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Specialty ->
                    defaultFields

                Labs ->
                    [ DropInput Required "Date/Time of Labs Collected" (defaultString newRecord.timeVisit) "TimeVisitId"
                    , DropInput Required "Date/Time of Labs Accessioned" (defaultString newRecord.timeAcc) "TimeAccId"
                    , TextInput Optional "Name of Lab" newRecord.title (UpdateTitle newRecord)
                    , TextInput Optional "Provider of Lab" newRecord.provider (UpdateProvider newRecord)
                    , AreaInput Required "Comments" newRecord.comments (UpdateComments newRecord)
                    , FileInput Required "Upload Record File" newRecord.fileName
                    ]

                Radiology ->
                    [ DropInput Required "Date/Time of Study was done" (defaultString newRecord.timeVisit) "TimeVisitId"
                    , DropInput Required "Date/Time of Study Accessioned" (defaultString newRecord.timeAcc) "TimeAccId"
                    , TextInput Optional "Name of Study" newRecord.title (UpdateTitle newRecord)
                    , TextInput Optional "Provider of Study" newRecord.provider (UpdateProvider newRecord)
                    , AreaInput Required "Comments" newRecord.comments (UpdateComments newRecord)
                    , FileInput Required "Upload Record File" newRecord.fileName
                    ]

                Misc ->
                    defaultFields

                Legal ->
                    [ TextInput Optional "Title" newRecord.title (UpdateTitle newRecord)
                    , AreaInput Required "Comments" newRecord.comments (UpdateComments newRecord)
                    , FileInput Required "Upload Record File" newRecord.fileName
                    ]

                Hospitalizations ->
                    []

                CallRecordings ->
                    [ TextInput Optional "Call Sid" newRecord.callSid (UpdateCallSid newRecord)
                    , TextInput Optional "Recording Sid" newRecord.recording (UpdateRecordingSid newRecord)
                    , NumrInput Required "Duration" newRecord.duration (UpdateDuration newRecord)
                    , DropInput Required "Recording Date" (defaultString newRecord.recordingDate) "RecordingDateId"
                    , DropInput Optional "User" (defaultInt newRecord.userId) "UserId"
                    , DropInput Optional "Task" (defaultInt newRecord.taskId) "TaskId"
                    ]

                PreviousHistories ->
                    [ DropInput Required "Report Date" (defaultString newRecord.reportDate) "ReportDateId"
                    , FileInput Required "Upload Record File" newRecord.fileName
                    ]

                Enrollment ->
                    [ TextInput Optional "Title" newRecord.title (UpdateTitle newRecord)
                    , AreaInput Required "Comments" newRecord.comments (UpdateComments newRecord)
                    , FileInput Required "Upload Record File" newRecord.fileName
                    ]
    in
        List.append firstColumns lastColumns


getColumns : Int -> List (Column RecordRow Msg)
getColumns recordTypeId =
    let
        recordType =
            getRecordType recordTypeId

        commonColumns =
            [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
            , stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
            , stringColumn "Specialty" (\t -> defaultString t.specialty)
            , stringColumn "Comments" (\t -> defaultString t.comments)
            ]

        firstColumns =
            case recordType of
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
                    , stringColumn "Comments" (\t -> defaultDateTime t.comments)
                    ]

                CallRecordings ->
                    [ stringColumn "Date" (\t -> dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> defaultString t.recording)
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
            [ editButton
            ]
    in
        List.append firstColumns lastColumns


config : Int -> Config RecordRow Msg
config recordTypeId =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordTypeId
        , customizations = defaultCustomizations
        }


defaultCustomizations : Customizations RecordRow msg
defaultCustomizations =
    { tableAttrs = defaultTableAttributes
    , caption = Nothing
    , thead = defaultThead
    , tfoot = Nothing
    , tbodyAttrs = []
    , rowAttrs = defaultRowAttrs .id
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


editButton : Column RecordRow msg
editButton =
    veryCustomColumn
        { name = ""
        , viewData = editButtonDiv << .id
        , sorter = unsortable
        }
