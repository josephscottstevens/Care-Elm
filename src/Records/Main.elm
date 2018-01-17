port module Records.Main exposing (..)

import Records.Load exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, button, ul, li, a)
import Html.Attributes exposing (class, id, type_, value, style)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)


-- import Utils.CommonGrid exposing (..)

import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions as Functions exposing (..)


port resetUpdate : Maybe Int -> Cmd msg


port sendMenuMessage : MenuMessage -> Cmd msg


port toggleConsent : Bool -> Cmd msg


port editTask : Int -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port resetUpdateComplete : (String -> msg) -> Sub msg


port updateFacility : (DropdownItem -> msg) -> Sub msg


port updateCategory : (DropdownItem -> msg) -> Sub msg


port updateTimeVisit : (Maybe String -> msg) -> Sub msg


port updateTimeAcc : (Maybe String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port updateReportDate : (Maybe String -> msg) -> Sub msg


port updateRecordingDate : (Maybe String -> msg) -> Sub msg


port updateUser : (DropdownItem -> msg) -> Sub msg


port updateTask : (DropdownItem -> msg) -> Sub msg


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
                , resetUpdateComplete ResetAddNew
                ]

        _ ->
            Sub.batch
                [ dropDownToggle DropDownToggle
                , deleteConfirmed DeleteConfirmed
                , resetUpdateComplete ResetAddNew
                ]


init : Flags -> Cmd Msg
init flag =
    Cmd.batch
        [ getRecords flag.patientId flag.recordType Load
        , sendMenuMessage (MenuMessage "Filter" 0 Nothing Nothing)
        ]


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
            { model | records = flipConsent model.records recordId model.recordTypeId } ! [ sendMenuMessage (getMenuMessage model recordId messageType) ]

        AddNewStart ->
            { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model False) ]

        ResetAddNew _ ->
            { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model True) ]

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
            case Functions.getResponseError responseMsg of
                Just t ->
                    model ! [ getRecords model.patientId model.recordTypeId Load, displayErrorMessage t ]

                Nothing ->
                    model
                        ! [ getRecords model.patientId model.recordTypeId Load
                          , displaySuccessMessage "Save completed successfully!"
                          , sendMenuMessage (MenuMessage "Filter" 0 Nothing Nothing)
                          ]

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
            case Functions.getResponseError responseMsg of
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

        UpdateRecordType newRecord dropdownItem ->
            if model.recordTypeId == dropdownItem.id then
                model ! []
            else
                { model | state = Limbo, recordTypeId = dropdownItem.id } ! [ resetUpdate dropdownItem.id, setLoadingStatus True ]

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

        UpdateFacility newRecord dropdownItem ->
            { model | state = AddNew { newRecord | facilityId = dropdownItem.id, facilityText = dropdownItem.name } } ! [ setUnsavedChanges True ]

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

        UpdateUser newRecord dropdownItem ->
            { model | state = AddNew { newRecord | userId = dropdownItem.id, userText = dropdownItem.name } } ! [ setUnsavedChanges True ]

        UpdateTask newRecord dropdownItem ->
            { model | state = AddNew { newRecord | taskId = dropdownItem.id, taskText = dropdownItem.name } } ! [ setUnsavedChanges True ]


view : Model -> Html Msg
view model =
    let
        rows =
            List.map (getRow model.recordTypeId) model.records
    in
        case model.state of
            Grid ->
                Table.view model.tableState rows gridConfig Nothing

            AddNew newRecord ->
                Table.view model.tableState rows gridConfig (Just <| viewNewRecord newRecord)

            Limbo ->
                div [] []

            Error errMessage ->
                div [] [ text errMessage ]


viewNewRecord : NewRecord -> Html Msg
viewNewRecord newRecord =
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
            class "btn btn-sm btn-default btn-success"

        footerControls =
            [ div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save newRecord), saveBtnClass ] [ text "Save" ]
                    , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
                    ]
                ]
            ]
    in
        div
            [ class "form-horizontal" ]
            (validationErrorsDiv :: inputControls ++ footerControls)


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


gridConfig : Config Msg
gridConfig =
    { domTableId = "RecordTable"
    , headers =
        [ "Date Collected"
        , "Doctor of Visit"
        , "Specialty"
        , "Comments"
        , ""
        ]
    , toolbar = [ ( "e-addnew", AddNewStart ) ]
    , toMsg = SetTableState
    }


getRow recordTypeId t =
    Row
        [ NullableDateTimeColumn "Date Collected" t .date (defaultSort .date)
        , NullableStringColumn "Doctor of Visit" t .provider (defaultSort .provider)
        , NullableStringColumn "Specialty" t .specialty (defaultSort .specialty)
        , NullableStringColumn "Comments" t .comments (defaultSort .comments)
        , DropdownColumn (dropdownItems recordTypeId t.id)
        ]
        t.id


dropdownItems : Maybe Int -> Int -> List ( String, String, Html.Attribute Msg )
dropdownItems recordTypeId rowId =
    case getRecordType recordTypeId of
        CallRecordings ->
            [ ( "e-edit", "Mark As Consent", onClick (SendMenuMessage rowId "MarkAsConsent") ) ]

        _ ->
            [ ( "e-sync", "Transfer", onClick (SendMenuMessage rowId "Transfer") )
            , ( "e-download", "View File", onClick (SendMenuMessage rowId "ViewFile") )
            , ( "e-mail", "Send By Email", onClick (SendMenuMessage rowId "SendByEmail") )
            , ( "e-print_01", "Send By Fax", onClick (SendMenuMessage rowId "SendByFax") )
            , ( "e-save", "Save To Client Portal", onClick (SendMenuMessage rowId "SaveToClientPortal") )
            , ( "e-contextdelete", "Delete", onClick (SendMenuMessage rowId "Delete") )
            ]



-- let
--     commonColumns =
--         [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--         , stringColumn "Doctor of Visit" (\t -> defaultString t.provider)
--         , stringColumn "Specialty" (\t -> defaultString t.specialty)
--         , stringColumn "Comments" (\t -> defaultString t.comments)
--         ]
--     firstColumns =
--         case getRecordType recordTypeId of
--             PrimaryCare ->
--                 commonColumns
--             Specialty ->
--                 commonColumns
--             Labs ->
--                 [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--                 , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
--                 , stringColumn "Name of Lab" (\t -> defaultString t.title)
--                 , stringColumn "Provider" (\t -> defaultString t.provider)
--                 , stringColumn "Comments" (\t -> defaultString t.comments)
--                 ]
--             Radiology ->
--                 [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--                 , stringColumn "Date Accessioned" (\t -> defaultDateTime t.dateAccessed)
--                 , stringColumn "Name of Study" (\t -> defaultString t.title)
--                 , stringColumn "Provider" (\t -> defaultString t.provider)
--                 , stringColumn "Comments" (\t -> defaultString t.comments)
--                 ]
--             Hospitalizations ->
--                 []
--             Legal ->
--                 [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--                 , stringColumn "Comments" (\t -> defaultString t.comments)
--                 ]
--             CallRecordings ->
--                 [ stringColumn "Date" (\t -> dateTime t.recordingDate)
--                 , hrefColumn "Recording" "Open" (\t -> defaultString t.recording)
--                 , hrefColumnExtra "Task" (\t -> defaultString t.taskTitle) "#" (OpenTask (defaultInt taskId))
--                 , checkColumn "During Enrollment" (\t -> t.enrollment)
--                 , checkColumn "Consent" (\t -> t.hasVerbalConsent)
--                 , stringColumn "User" (\t -> defaultString t.staffName)
--                 ]
--             PreviousHistories ->
--                 [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--                 , stringColumn "File Name" (\t -> defaultString t.fileName)
--                 , stringColumn "Report Date" (\t -> defaultDate t.reportDate)
--                 , stringColumn "Comments" (\t -> defaultString t.comments)
--                 ]
--             Enrollment ->
--                 [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
--                 , stringColumn "Comments" (\t -> defaultString t.comments)
--                 ]
--             Misc ->
--                 commonColumns
--     lastColumns =
--         [ rowDropDown recordTypeId
--         ]
-- in
--     List.append firstColumns lastColumns
-- rowDropDown : Maybe Int -> Table.Column RecordRow Msg
-- rowDropDown recordTypeId =
--     Table.veryCustomColumn
--         { name = ""
--         , viewData = (\t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropdownItems recordTypeId t.id))
--         , sorter = Table.unsortable
--         }
-- config : Maybe Int -> Maybe Int -> Table.Config RecordRow Msg
-- config recordTypeId taskId =
--     customConfig
--         { toId = \t -> toString t.id
--         , toMsg = SetTableState
--         , columns = getColumns recordTypeId taskId .id
--         , customizations =
--             { defaultCustomizations
--                 | tableAttrs = standardTableAttrs "RecordTable"
--                 , thead = standardThead
--             }
--         }
