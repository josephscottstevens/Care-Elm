port module Records exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, h4, button)
import Html.Attributes exposing (class, type_, id, value)
import Html.Events exposing (onClick)
import Table exposing (stringColumn, dateColumn, intColumn, dateTimeColumn, dropdownColumn, hrefColumn, checkColumn)
import Common.Types as Common exposing (RequiredType(Required, Optional), AddEditDataSource, RecordType, DropdownItem)
import Common.Functions as Functions exposing (sendMenuMessage, displaySuccessMessage, displayErrorMessage, maybeVal, defaultString, maybeToDateString)
import Common.Html
    exposing
        ( getValidationErrors
        , defaultConfig
        , makeControls
        , fullWidth
        , InputControlType(DropInput, TextInput, AreaInput, DateInput, FileInput, DropInputWithButton, KnockInput, CheckInput, NumrInput)
        )
import Http
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


port initRecordAddNew : EditData -> Cmd msg


port updateRecordAddNew : (EditData -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port editTask : Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteConfirmed
        , updateRecordAddNew UpdateRecordAddNew
        ]


type ModelState
    = Grid
    | AddNew EditData
    | Limbo
    | Error String


init : Maybe Int -> Int -> Cmd Msg
init recordTypeId patientId =
    case recordTypeId of
        Just t ->
            Cmd.batch
                [ loadRecords t patientId
                , getDropDowns patientId AddEditDataSourceLoaded
                ]

        Nothing ->
            displayErrorMessage "Invalid recordTypeId"


type alias Model =
    { state : ModelState
    , rows : List RecordRow
    , dropDownState : Int
    , tableState : Table.State

    -- Hospitilizations
    , isExistingHospitilization : Bool
    , patientReported : Bool
    , dischargeDiagnosis : String

    -- Edit
    , editData : Maybe EditData
    , recordId : Maybe Int
    , addEditDataSource : Maybe AddEditDataSource
    , title : String
    , specialty : String
    , provider : String
    , comments : String
    , showValidationErrors : Bool
    , recording : String
    , callSid : String
    , duration : Int
    }


type alias RecordRow =
    { id : Int
    , date : Maybe String
    , specialty : Maybe String
    , comments : Maybe String
    , transferedTo : Maybe String
    , patientId : Int
    , title : Maybe String
    , dateAccessed : Maybe String
    , provider : Maybe String
    , recordType : Maybe String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dischargePhysician : Maybe String
    , dischargeDiagnosis : Maybe String
    , hospitalizationServiceType : Maybe String
    , hospitalizationId : Maybe Int
    , reportDate : Maybe String
    , fileName : Maybe String
    , recommendations : Maybe String
    , taskId : Maybe Int
    , taskTitle : Maybe String
    , recording : Maybe String
    , recordingDate : Maybe String
    , recordingDuration : Int
    , enrollment : Bool
    , staffId : Int
    , staffName : Maybe String
    , hasVerbalConsent : Bool
    }


type alias EditData =
    { facilityId : Maybe Int
    , facilities : List DropdownItem
    , recordTypes : List DropdownItem
    , users : List DropdownItem
    , tasks : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , hospitilizations : List DropdownItem

    -- tt
    , timeVisit : Maybe String
    , timeAcc : Maybe String
    , fileName : String
    , facilityId : Maybe Int
    , facilityText : String
    , reportDate : Maybe String
    , recordingDate : Maybe String
    , userId : Maybe Int
    , userText : String
    , taskId : Maybe Int
    , taskText : String
    , hospitalizationId : Maybe Int
    , hospitalizationText : String
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    }


view : Model -> Maybe Int -> Html Msg
view model recordTypeId =
    -- let
    --     rows =
    --         model.rows
    --     -- |> sort model.tableState (List.map (getColumns model.recordTypeId) model.rows)
    --     -- |> List.map (getRow model.recordTypeId)
    -- in
    let
        config =
            (gridConfig recordTypeId model.addEditDataSource)
    in
        case model.state of
            Grid ->
                Table.view model.tableState model.rows config Nothing

            AddNew newRecord ->
                Table.view model.tableState model.rows config (Just <| viewNewRecord model newRecord recordTypeId)

            Limbo ->
                div [] []

            Error errMessage ->
                div [] [ text errMessage ]


viewNewRecord : Model -> EditData -> Maybe Int -> Html Msg
viewNewRecord model newRecord recordTypeId =
    let
        inputControls : List (Html Msg)
        inputControls =
            [ makeControls defaultConfig (formInputs model newRecord recordTypeId) ]

        errors =
            getValidationErrors (formInputs model newRecord recordTypeId)

        validationErrorsDiv =
            if model.showValidationErrors == True && List.length errors > 0 then
                div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
            else
                div [] []

        saveBtnClass =
            class "btn btn-sm btn-default btn-success"

        footerControls : List (Html Msg)
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


type Msg
    = Load (Result Http.Error (List RecordRow))
    | SetTableState Table.State
    | Add AddEditDataSource
    | SendMenuMessage Common.RecordType String Int
    | EditTask Int
    | DeletePrompt Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | AddEditDataSourceLoaded (Result Http.Error AddEditDataSource)
    | NoOp
      -- Edit Messages
    | AddNewFacility
    | AddNewPhysician
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateRecordAddNew EditData
    | UpdateTitle String
    | UpdateSpecialty String
    | UpdateProvider String
    | UpdateComments String
    | UpdateCallSid String
    | UpdateRecordingSid String
    | UpdateDuration String
      -- Hospitilizations
    | UpdateIsExistingHospitilization Bool
    | UpdatePatientReported Bool
    | UpdateDischargeDiagnosis String


update : Msg -> Model -> Int -> Maybe Int -> ( Model, Cmd Msg )
update msg model patientId recordTypeId =
    let
        updateAddNew t =
            t ! [ Functions.setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                { model | rows = t } ! [ Functions.setLoadingStatus False ]

            Load (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            Add addEditDataSource ->
                { model | state = AddNew (getEditData addEditDataSource recordTypeId) }
                    ! [ initRecordAddNew (getEditData addEditDataSource recordTypeId) ]

            SetTableState newState ->
                { model | tableState = newState } ! []

            SendMenuMessage recordType messageType recordId ->
                { model | rows = flipConsent model.rows recordId recordType }
                    ! [ sendMenuMessage (getMenuMessage model.rows recordType recordId messageType) ]

            DeletePrompt rowId ->
                model ! [ Functions.deletePrompt rowId ]

            DeleteConfirmed rowId ->
                { model | rows = model.rows |> List.filter (\t -> t.id /= rowId) }
                    ! [ Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)
                            |> Http.send DeleteCompleted
                      ]

            DeleteCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Record deleted successfully!" ]

            DeleteCompleted (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            EditTask taskId ->
                model ! [ editTask taskId ]

            AddEditDataSourceLoaded response ->
                case response of
                    Ok t ->
                        { model | addEditDataSource = Just t } ! []

                    Err t ->
                        model ! [ displayErrorMessage ("Error loading edit data source" ++ toString t) ]

            NoOp ->
                model ! []

            -- Edit
            AddNewFacility ->
                model ! [ addNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addNewPhysician Nothing ]

            Save editData ->
                if List.length (getValidationErrors (formInputs model editData recordTypeId)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    model
                        ! [ "/People/AddNewRecord"
                                |> Functions.postRequest (encodeRecord model editData patientId recordTypeId)
                                |> Http.send SaveCompleted
                          , Functions.setUnsavedChanges False
                          ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        { model | state = Grid } ! [ displayErrorMessage t ]

                    Nothing ->
                        { model | state = Grid }
                            ! [ displaySuccessMessage "Save completed successfully!"
                              , case recordTypeId of
                                    Just recordTypeId ->
                                        loadRecords recordTypeId patientId

                                    Nothing ->
                                        Cmd.none
                              ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel ->
                { model | state = Grid } ! [ Functions.setUnsavedChanges False ]

            UpdateRecordAddNew editData ->
                { model | editData = Just editData } ! []

            UpdateTitle str ->
                updateAddNew { model | title = str }

            UpdateSpecialty str ->
                updateAddNew { model | specialty = str }

            UpdateProvider str ->
                updateAddNew { model | provider = str }

            UpdateComments str ->
                updateAddNew { model | comments = str }

            UpdateCallSid str ->
                updateAddNew { model | callSid = str }

            UpdateRecordingSid str ->
                updateAddNew { model | recording = str }

            UpdateDuration str ->
                updateAddNew { model | duration = Functions.defaultIntStr str }

            -- Hospitilizations
            UpdateIsExistingHospitilization bool ->
                model ! []

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateDischargeDiagnosis str ->
                updateAddNew { model | dischargeDiagnosis = str }


getColumns : Maybe Int -> List (Table.Column RecordRow Msg)
getColumns recordTypeId =
    case Functions.getRecordTypeById recordTypeId of
        Just recordType ->
            let
                commonColumns =
                    [ dateTimeColumn "Date Collected" .date
                    , stringColumn "Doctor of Visit" .provider
                    , stringColumn "Specialty" .specialty
                    , stringColumn "Comments" .comments
                    ]

                firstColumns =
                    case recordType of
                        Common.PrimaryCare ->
                            commonColumns

                        Common.Specialty ->
                            commonColumns

                        Common.Labs ->
                            [ dateTimeColumn "Date Collected" .date
                            , dateTimeColumn "Date Accessioned" .dateAccessed
                            , stringColumn "Name of Lab" .title
                            , stringColumn "Provider" .provider
                            , stringColumn "Comments" .comments
                            ]

                        Common.Radiology ->
                            [ dateTimeColumn "Date Collected" .date
                            , dateTimeColumn "Date Accessioned" .dateAccessed
                            , stringColumn "Name of Study" .title
                            , stringColumn "Provider" .provider
                            , stringColumn "Comments" .comments
                            ]

                        Common.Hospitalizations ->
                            [ dateTimeColumn "Date Collected" .date
                            , intColumn "Hospitalization ID" .hospitalizationId
                            , dateTimeColumn "Admin Collected" .dateOfAdmission
                            , dateTimeColumn "Discharge Date" .dateOfDischarge
                            , stringColumn "Service Type" .hospitalizationServiceType
                            , stringColumn "Discharge Recommendations" .recommendations
                            , stringColumn "Discharge Physician" .dischargePhysician
                            , stringColumn "Comments" .comments
                            ]

                        Common.Legal ->
                            [ dateTimeColumn "Date Collected" .date
                            , stringColumn "Comments" .comments
                            ]

                        Common.CallRecordings ->
                            [ dateColumn "Date" .recordingDate
                            , hrefColumn "Recording" "Open" .recording

                            -- , hrefCustom
                            , checkColumn "During Enrollment" .enrollment
                            , checkColumn "Consent" .hasVerbalConsent
                            , stringColumn "User" .staffName
                            ]

                        Common.PreviousHistories ->
                            [ dateTimeColumn "Date Collected" .date
                            , stringColumn "File Name" .fileName
                            , dateColumn "Report Date" .reportDate
                            , stringColumn "Comments" .comments
                            ]

                        Common.Enrollment ->
                            [ dateTimeColumn "Date Collected" .date
                            , stringColumn "Comments" .comments
                            ]

                        Common.Misc ->
                            commonColumns

                lastColumns =
                    [ dropdownColumn (dropdownItems recordTypeId)
                    ]
            in
                List.append firstColumns lastColumns

        Nothing ->
            []


decodeRecordRow : Decoder RecordRow
decodeRecordRow =
    decode RecordRow
        |> required "Id" Decode.int
        |> required "Date" (maybe Decode.string)
        |> required "Specialty" (maybe Decode.string)
        |> required "Comments" (maybe Decode.string)
        |> required "TransferedTo" (maybe Decode.string)
        |> required "PatientId" Decode.int
        |> required "Title" (maybe Decode.string)
        |> required "DateAccessed" (maybe Decode.string)
        |> required "Provider" (maybe Decode.string)
        |> required "RecordType" (maybe Decode.string)
        |> required "DateOfAdmission" (maybe Decode.string)
        |> required "DateOfDischarge" (maybe Decode.string)
        |> required "DischargePhysician" (maybe Decode.string)
        |> required "DischargeDiagnosis" (maybe Decode.string)
        |> required "HospitalizationServiceType" (maybe Decode.string)
        |> required "HospitalizationId" (maybe Decode.int)
        |> required "ReportDate" (maybe Decode.string)
        |> required "FileName" (maybe Decode.string)
        |> required "Recommendations" (maybe Decode.string)
        |> required "TaskId" (maybe Decode.int)
        |> required "TaskTitle" (maybe Decode.string)
        |> required "Recording" (maybe Decode.string)
        |> required "RecordingDate" (maybe Decode.string)
        |> required "RecordingDuration" Decode.int
        |> required "Enrollment" Decode.bool
        |> required "StaffId" Decode.int
        |> required "StaffName" (maybe Decode.string)
        |> required "HasVerbalConsent" Decode.bool


loadRecords : Int -> Int -> Cmd Msg
loadRecords recordTypeId patientId =
    let
        url =
            "/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId
    in
        Decode.field "list" (Decode.list decodeRecordRow)
            |> Http.get url
            |> Http.send Load


getMenuMessage : List RecordRow -> Common.RecordType -> Int -> String -> Common.MenuMessage
getMenuMessage rows recordType recordId messageType =
    let
        maybeVerbalConsent =
            rows
                |> List.filter (\t -> t.id == recordId)
                |> List.head
                |> Maybe.map (\t -> not t.hasVerbalConsent)

        recordTypeId =
            Just <| Functions.getId recordType
    in
        Common.MenuMessage messageType recordId recordTypeId maybeVerbalConsent


flipConsent : List RecordRow -> Int -> Common.RecordType -> List RecordRow
flipConsent rows recordId recordType =
    case recordType of
        Common.CallRecordings ->
            rows
                |> List.map
                    (\t ->
                        if t.id == recordId then
                            { t | hasVerbalConsent = not t.hasVerbalConsent }
                        else
                            t
                    )

        _ ->
            rows


formInputs : Model -> EditData -> Maybe Int -> List (InputControlType Msg)
formInputs model editData recordTypeId =
    let
        firstColumns =
            [ DropInput "Facility" Required editData.facilityId "FacilityId"
            ]

        lastControls =
            [ AreaInput "Comments" Required model.comments UpdateComments
            , FileInput "Upload Record File" Required editData.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString editData.timeVisit) "TimeVisitId"
                   , TextInput "Doctor of Visit" Optional model.provider UpdateProvider
                   , TextInput "Specialty of Visit" Optional model.specialty UpdateSpecialty
                   ]
                ++ lastControls

        columns =
            case Functions.getRecordTypeById recordTypeId of
                Just Common.PrimaryCare ->
                    defaultFields

                Just Common.Specialty ->
                    defaultFields

                Just Common.Labs ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString editData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString editData.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional model.title UpdateTitle
                           , TextInput "Provider of Lab" Optional model.provider UpdateProvider
                           ]
                        ++ lastControls

                Just Common.Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString editData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString editData.timeAcc) "TimeAccId"
                           , TextInput "Name of Study" Optional model.title UpdateTitle
                           , TextInput "Provider of Study" Optional model.provider UpdateProvider
                           ]
                        ++ lastControls

                Just Common.Misc ->
                    defaultFields

                Just Common.Legal ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastControls

                Just Common.Hospitalizations ->
                    case model.isExistingHospitilization of
                        True ->
                            [ CheckInput "Existing Hospitilization" Common.Optional model.isExistingHospitilization UpdateIsExistingHospitilization
                            , DropInput "Select Hospitalization" Common.Required editData.hospitalizationId "HospitalizationsId"
                            ]
                                ++ lastControls

                        False ->
                            [ CheckInput "Patient Reported" Common.Optional model.patientReported UpdatePatientReported
                            , DropInputWithButton
                                "Facility"
                                Common.Optional
                                editData.facilityId
                                "FacilityId"
                                "Add New Facility"
                            , DateInput "Date of Admission" Required (defaultString editData.dateOfAdmission) "DateOfAdmissionId"
                            , DateInput "Date of Discharge" Required (defaultString editData.dateOfDischarge) "DateOfDischargeId"
                            , DropInput "Hospital Service Type" Required editData.hospitalServiceTypeId "HospitalServiceTypeId"
                            , AreaInput "Chief Complaint" Required model.comments UpdateComments
                            , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
                            , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
                            , TextInput "Discharge Recommendations" Required model.dischargeDiagnosis UpdateDischargeDiagnosis
                            , DropInputWithButton "Discharge Physician"
                                Optional
                                editData.dischargePhysicianId
                                "DischargePhysicianId"
                                "New Provider"
                            , DropInputWithButton
                                "Secondary Facility Name"
                                Optional
                                editData.facilityId2
                                "FacilityId2"
                                "Add New Facility"
                            , DateInput "Secondary Date of Admission" Optional (defaultString editData.dateOfAdmission) "DateOfAdmissionId2"
                            , DateInput "Secondary Date of Discharge" Optional (defaultString editData.dateOfDischarge) "DateOfDischargeId2"
                            , FileInput "Upload Record File" Required editData.fileName
                            ]
                                ++ lastControls

                Just Common.CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required model.callSid UpdateCallSid
                           , TextInput "Recording Sid" Required model.recording UpdateRecordingSid
                           , NumrInput "Duration" Required model.duration UpdateDuration
                           , DateInput "Recording Date" Required (defaultString editData.recordingDate) "RecordingDateId"
                           , DropInput "User" Required editData.userId "UserId"
                           , DropInput "Task" Optional editData.taskId "TaskId"
                           ]

                Just Common.PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString editData.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required editData.fileName
                           ]

                Just Common.Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastControls

                Nothing ->
                    []
    in
        columns


gridConfig : Maybe Int -> Maybe AddEditDataSource -> Table.Config RecordRow Msg
gridConfig recordTypeId addEditDataSource =
    { domTableId = "RecordTable"
    , headers =
        [ "Date Collected"
        , "Doctor of Visit"
        , "Specialty"
        , "Comments"
        , ""
        ]
    , toolbar =
        case addEditDataSource of
            Just t ->
                [ ( "e-addnew", Add t ) ]

            Nothing ->
                [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns = getColumns recordTypeId
    }


dropdownItems : Maybe Int -> List ( String, String, Int -> Msg )
dropdownItems recordTypeId =
    case Functions.getRecordTypeById recordTypeId of
        Just recordType ->
            case recordType of
                Common.CallRecordings ->
                    [ ( "e-edit", "Mark As Consent", SendMenuMessage recordType "MarkAsConsent" ) ]

                _ ->
                    [ ( "e-sync", "Transfer", (SendMenuMessage recordType "Transfer") )
                    , ( "e-download", "View File", (SendMenuMessage recordType "ViewFile") )
                    , ( "e-mail", "Send By Email", (SendMenuMessage recordType "SendByEmail") )
                    , ( "e-print_01", "Send By Fax", (SendMenuMessage recordType "SendByFax") )
                    , ( "e-save", "Save To Client Portal", (SendMenuMessage recordType "SaveToClientPortal") )
                    , ( "e-contextdelete", "Delete", (SendMenuMessage recordType "Delete") )
                    ]

        Nothing ->
            []


encodeRecord : Model -> EditData -> Int -> Maybe Int -> Encode.Value
encodeRecord newRecord editData patientId recordTypeId =
    Encode.object
        [ ( "RecordId", maybeVal Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", maybeVal Encode.int recordTypeId )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| editData.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| editData.timeAcc )
        , ( "RecordFile", Encode.string <| editData.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| editData.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| editData.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| editData.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| editData.userId )
        , ( "TaskId", maybeVal Encode.int <| editData.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| editData.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| editData.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| editData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| editData.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| editData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| editData.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| editData.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeDiagnosis )
        , ( "DischargePhysicianId", maybeVal Encode.int <| editData.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| editData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| editData.dischargeDiagnosisId )
        ]


emptyModel : Model
emptyModel =
    { state = Grid
    , addEditDataSource = Nothing
    , title = ""
    , specialty = ""
    , provider = ""
    , comments = ""
    , showValidationErrors = False
    , recording = ""
    , callSid = ""
    , duration = 0

    -- Hospitilizations
    , recordId = Nothing
    , editData = Nothing
    , isExistingHospitilization = False
    , patientReported = False
    , dischargeDiagnosis = ""
    , rows = []
    , tableState = Table.init "Date"
    , dropDownState = -1
    }


getEditData : AddEditDataSource -> Maybe Int -> EditData
getEditData addEditDataSource recordTypeId =
    case Functions.getRecordTypeById recordTypeId of
        Just t ->
            { facilityId = addEditDataSource.facilityId
            , facilities = addEditDataSource.facilities
            , recordTypes = addEditDataSource.recordTypes
            , users = addEditDataSource.users
            , tasks = addEditDataSource.tasks
            , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
            , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
            , hospitilizations = addEditDataSource.hospitilizations

            -- no data from server, just filler data
            , timeVisit = Nothing
            , timeAcc = Nothing
            , fileName = ""
            , facilityText = ""
            , reportDate = Nothing
            , recordingDate = Nothing
            , userId = Nothing
            , userText = ""
            , taskId = Nothing
            , taskText = ""
            , hospitalizationId = Nothing
            , hospitalizationText = ""
            , facilityId2 = Nothing
            , facilityText2 = ""
            , dateOfAdmission = Nothing
            , dateOfDischarge = Nothing
            , dateOfAdmission2 = Nothing
            , dateOfDischarge2 = Nothing
            , hospitalServiceTypeId = Nothing
            , hospitalServiceTypeText = ""
            , admitDiagnosisId = Nothing
            , dischargeDiagnosisId = Nothing
            , dischargePhysicianId = Nothing
            , dischargePhysicianText = ""
            }

        Nothing ->
            Debug.crash "whoops"


getDropDowns : Int -> (Result Http.Error AddEditDataSource -> msg) -> Cmd msg
getDropDowns patientId t =
    decode AddEditDataSource
        |> required "facilityId" (maybe Decode.int)
        |> required "patientId" Decode.int
        |> required "facilityDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "providersDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "recordTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "userDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "taskDropDown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizationServiceTypeDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitalizationDischargePhysicianDropdown" (Decode.list Functions.decodeDropdownItem)
        |> required "hospitilizations" (Decode.list Functions.decodeDropdownItem)
        |> Http.get ("/People/PatientRecordsDropdowns?patientId=" ++ toString patientId)
        |> Http.send t
