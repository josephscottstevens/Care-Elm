port module Records.Main exposing (..)

import Records.Functions exposing (..)
import Records.Model exposing (..)
import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick, onFocus)
import Table exposing (..)
import Utils.CommonGrid exposing (..)
import Utils.CommonHtml exposing (..)
import Utils.CommonTypes exposing (..)
import Utils.CommonFunctions exposing (..)


port resetUpdate : Maybe Int -> Cmd msg


port sendMenuMessage : MenuMessage -> Cmd msg


port toggleConsent : Bool -> Cmd msg


port editTask : Int -> Cmd msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port initSyncfusionControls : SyncFusionMessage -> Cmd msg


port displaySuccessMessage : String -> Cmd msg


port displayErrorMessage : String -> Cmd msg


port setLoadingStatus : Bool -> Cmd msg


port setUnsavedChanges : Bool -> Cmd msg


port resetUpdateComplete : (String -> msg) -> Sub msg


port updateFacility : (DropDownItem -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port updateTimeVisit : (Maybe String -> msg) -> Sub msg


port updateTimeAcc : (Maybe String -> msg) -> Sub msg


port updateFileName : (String -> msg) -> Sub msg


port updateReportDate : (Maybe String -> msg) -> Sub msg


port updateRecordingDate : (Maybe String -> msg) -> Sub msg


port updateUser : (DropDownItem -> msg) -> Sub msg


port updateTask : (DropDownItem -> msg) -> Sub msg



-- Hospitilizations


port updateFacility2 : (DropDownItem -> msg) -> Sub msg


port updateDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateDischargePhysician : (DropDownItem -> msg) -> Sub msg


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

                -- Hospitilizations
                , updateFacility2 (UpdateFacility2 t)
                , updateDateOfAdmission (UpdateDateOfAdmission t)
                , updateDateOfDischarge (UpdateDateOfDischarge t)
                , updateHospitalServiceType (UpdateHospitalServiceType t)
                , updateDischargePhysician (UpdateDischargePhysician t)
                ]

        _ ->
            Sub.batch
                [ dropDownToggle DropDownToggle
                , deleteConfirmed DeleteConfirmed
                , resetUpdateComplete ResetAddNew
                ]


init : Flags -> Cmd Msg
init flag =
    getRecords flag.patientId flag.recordType Load


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateAddNew t =
            { model | state = AddNew t } ! [ setUnsavedChanges True ]
    in
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
                { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model False) ]

            ResetAddNew _ ->
                { model | state = AddNew (getNewRecord model) } ! [ initSyncfusionControls (getSyncFusionMessage model True) ]

            Save newRecord ->
                if List.length (getValidationErrors (formInputs newRecord)) > 0 then
                    updateAddNew { newRecord | showValidationErrors = True }
                else
                    model ! [ saveForm newRecord, setUnsavedChanges False ]

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
                updateAddNew { newRecord | title = str }

            EditTask taskId ->
                model ! [ editTask taskId ]

            AddNewFacility ->
                model ! [ addNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addNewPhysician Nothing ]

            UpdateRecordType newRecord dropDownItem ->
                if model.recordTypeId == dropDownItem.id then
                    model ! []
                else
                    { model | state = Limbo, recordTypeId = dropDownItem.id } ! [ resetUpdate dropDownItem.id, setLoadingStatus True ]

            SetFilter filterState ->
                { model | filterFields = filterFields model.filterFields filterState } ! []

            UpdateSpecialty newRecord str ->
                updateAddNew { newRecord | specialty = str }

            UpdateProvider newRecord str ->
                updateAddNew { newRecord | provider = str }

            UpdateTimeVisit newRecord str ->
                updateAddNew { newRecord | timeVisit = str }

            UpdateTimeAcc newRecord str ->
                updateAddNew { newRecord | timeAcc = str }

            UpdateFileName newRecord str ->
                updateAddNew { newRecord | fileName = str }

            UpdateComments newRecord str ->
                updateAddNew { newRecord | comments = str }

            UpdateFacility newRecord dropDownItem ->
                updateAddNew { newRecord | facilityId = dropDownItem.id, facilityText = dropDownItem.name }

            UpdateReportDate newRecord str ->
                updateAddNew { newRecord | reportDate = str }

            UpdateCallSid newRecord str ->
                updateAddNew { newRecord | callSid = str }

            UpdateRecordingSid newRecord str ->
                updateAddNew { newRecord | recording = str }

            UpdateDuration newRecord str ->
                updateAddNew { newRecord | duration = defaultIntStr str }

            UpdateRecordingDate newRecord str ->
                updateAddNew { newRecord | recordingDate = str }

            UpdateUser newRecord dropDownItem ->
                updateAddNew { newRecord | userId = dropDownItem.id, userText = dropDownItem.name }

            UpdateTask newRecord dropDownItem ->
                updateAddNew { newRecord | taskId = dropDownItem.id, taskText = dropDownItem.name }

            -- Hospitilizations
            UpdateFacility2 newRecord dropDownItem ->
                updateAddNew { newRecord | facilityId2 = dropDownItem.id, facilityText2 = dropDownItem.name }

            UpdateDateOfAdmission newRecord str ->
                updateAddNew { newRecord | dateOfAdmission = str }

            UpdateDateOfDischarge newRecord str ->
                updateAddNew { newRecord | dateOfDischarge = str }

            UpdateHospitalServiceType newRecord dropDownItem ->
                updateAddNew { newRecord | hospitalServiceTypeId = dropDownItem.id, hospitalServiceTypeText = dropDownItem.name }

            UpdateDischargeRecommendations newRecord str ->
                updateAddNew { newRecord | dischargeRecommendations = str }

            UpdateDischargePhysician newRecord dropDownItem ->
                updateAddNew { newRecord | dischargePhysicianId = dropDownItem.id, dischargePhysicianText = dropDownItem.name }


view : Model -> Html Msg
view model =
    case model.state of
        Grid ->
            div []
                [ button [ type_ "button", class "btn btn-sm btn-default margin-bottom-5", onClick AddNewStart ] [ text "New Record" ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config SetFilter model.recordTypeId (updateTaskId model)) model.tableState (filteredRecords model) ]
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
                    class "btn btn-sm btn-success margin-left-5 pull-right"

                footerControls =
                    [ div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save newRecord), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick Cancel, class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]
            in
                div
                    [ class "form-horizontal" ]
                    (validationErrorsDiv :: inputControls ++ footerControls)

        Limbo ->
            div [] []

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
            , ( "Category", Required, DropInput newRecord.recordTypeId "CategoryId" )
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
                    case newRecord.hospitalizationId of
                        Just t ->
                            []

                        Nothing ->
                            [ ( "Facility", Required, DropInputWithButton newRecord.facilityId2 "FacilityId2" AddNewFacility "Add New Facility" )
                            , ( "Date of Admission", Required, DateInput (defaultString newRecord.dateOfAdmission) "DateOfAdmissionId" (UpdateDateOfAdmission newRecord) )
                            , ( "Date of Discharge", Required, DateInput (defaultString newRecord.dateOfDischarge) "DateOfDischargeId" (UpdateDateOfDischarge newRecord) )
                            , ( "Hospital Service Type", Required, DropInput newRecord.hospitalServiceTypeId "HospitalServiceTypeId" )
                            , ( "Discharge Recommendations", Required, TextInput newRecord.dischargeRecommendations (UpdateDischargeRecommendations newRecord) )
                            , ( "Discharge Physician", Required, DropInputWithButton newRecord.dischargePhysicianId "DischargePhysicianId" AddNewPhysician "Add New Physician" )
                            , ( "Comments", Required, AreaInput newRecord.comments (UpdateComments newRecord) )
                            , ( "Upload Record File", Required, FileInput newRecord.fileName )
                            ]

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


getColumns : Maybe Int -> Maybe Int -> List (Column RecordRow Msg)
getColumns recordTypeId taskId =
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
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Hospitalization ID" (\t -> defaultIntToString t.hospitalizationId)
                    , stringColumn "Admin Date" (\t -> defaultDateTime t.dateOfAdmission)
                    , stringColumn "Discharge Date" (\t -> defaultDateTime t.dateOfDischarge)
                    , stringColumn "Service Type" (\t -> defaultString t.hospitalizationServiceType)
                    , stringColumn "Discharge Recommendations" (\t -> defaultString t.recommendations)
                    , stringColumn "Discharge Physician" (\t -> defaultString t.dischargePhysician)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                Legal ->
                    [ stringColumn "Date Collected" (\t -> defaultDateTime t.date)
                    , stringColumn "Comments" (\t -> defaultString t.comments)
                    ]

                CallRecordings ->
                    [ stringColumn "Date" (\t -> dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> defaultString t.recording)
                    , hrefColumnExtra "Task" (\t -> defaultString t.taskTitle) "#" (EditTask (defaultInt taskId))
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
            [ rowDropDownColumn recordTypeId
            ]
    in
        List.append firstColumns lastColumns


rowDropDownColumn : Maybe Int -> Table.Column RecordRow Msg
rowDropDownColumn recordTypeId =
    Table.veryCustomColumn
        { name = ""
        , viewData = (\t -> rowDropDownDiv t.dropDownOpen (onClick (DropDownToggle t.id)) (dropDownItems recordTypeId t.id))
        , sorter = Table.unsortable
        }


config : (FilterState -> Msg) -> Maybe Int -> Maybe Int -> Config RecordRow Msg
config msg recordTypeId taskId =
    customConfig
        { toId = \t -> toString t.id
        , toMsg = SetTableState
        , columns = getColumns recordTypeId taskId
        , customizations =
            { defaultCustomizations | tableAttrs = standardTableAttrs "RecordTable", thead = (standardThead msg) }
        }


dropDownItems : Maybe Int -> Int -> List ( String, String, Html.Attribute Msg )
dropDownItems recordTypeId rowId =
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
