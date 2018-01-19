port module Records exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, type_, id, value)
import Html.Events exposing (onClick)
import Table exposing (stringColumn, dateColumn, intColumn, dateTimeColumn, dropdownColumn, hrefColumn, hrefColumnExtra, checkColumn)
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


port initRecordAddNew : SfData -> Cmd msg


port updateRecordAddNew : (SfData -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port editTask : Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteConfirmed
        , updateRecordAddNew UpdateRecordAddNew
        ]


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
    { rows : List RecordRow
    , dropDownState : Int
    , tableState : Table.State
    , addEditDataSource : Maybe AddEditDataSource
    , editData : Maybe EditData
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


type alias SfData =
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


type alias EditData =
    { recordType : RecordType
    , isExistingHospitilization : Bool
    , patientReported : Bool
    , dischargeDiagnosis : String

    -- Hospitilizations
    , showValidationErrors : Bool
    , recordId : Maybe Int
    , title : String
    , specialty : String
    , provider : String
    , comments : String
    , recording : String
    , callSid : String
    , duration : Int
    , sfData : SfData
    }


view : Model -> Maybe Int -> Html Msg
view model recordTypeId =
    case Functions.getRecordTypeById recordTypeId of
        Just recordType ->
            let
                config =
                    gridConfig recordType model.addEditDataSource
            in
                case model.editData of
                    Just editData ->
                        Table.view model.tableState model.rows config (Just <| viewEditData editData)

                    Nothing ->
                        Table.view model.tableState model.rows config Nothing

        Nothing ->
            text "Error invalid recordState"


viewEditData : EditData -> Html Msg
viewEditData editData =
    let
        inputControls : List (Html Msg)
        inputControls =
            [ makeControls defaultConfig (formInputs editData) ]

        errors =
            getValidationErrors (formInputs editData)

        validationErrorsDiv =
            if editData.showValidationErrors == True && List.length errors > 0 then
                div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
            else
                div [] []

        saveBtnClass =
            class "btn btn-sm btn-default btn-success"

        footerControls : List (Html Msg)
        footerControls =
            [ div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [ button [ type_ "button", id "Save", value "AddNewRecord", onClick (Save editData), saveBtnClass ] [ text "Save" ]
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
    | Add AddEditDataSource RecordType
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
    | SaveCompleted RecordType (Result Http.Error String)
    | Cancel
    | UpdateRecordAddNew SfData
    | UpdateTitle EditData String
    | UpdateSpecialty EditData String
    | UpdateProvider EditData String
    | UpdateComments EditData String
    | UpdateCallSid EditData String
    | UpdateRecordingSid EditData String
    | UpdateDuration EditData String
      -- Hospitilizations
    | UpdateIsExistingHospitilization EditData Bool
    | UpdatePatientReported EditData Bool
    | UpdateDischargeDiagnosis EditData String


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            { model | editData = Just t } ! [ Functions.setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                { model | rows = t } ! [ Functions.setLoadingStatus False ]

            Load (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            Add addEditDataSource recordType ->
                let
                    editData =
                        createEditData addEditDataSource recordType
                in
                    { model | editData = Just editData }
                        ! [ initRecordAddNew editData.sfData ]

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
                if List.length (getValidationErrors (formInputs editData)) > 0 then
                    { model | editData = Just { editData | showValidationErrors = True } } ! []
                else
                    model
                        ! [ "/People/AddNewRecord"
                                |> Functions.postRequest (encodeRecord editData patientId)
                                |> Http.send (SaveCompleted editData.recordType)
                          , Functions.setUnsavedChanges False
                          ]

            SaveCompleted recordType (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        { model | editData = Nothing } ! [ displayErrorMessage t ]

                    Nothing ->
                        { model | editData = Nothing }
                            ! [ displaySuccessMessage "Save completed successfully!"
                              , loadRecords (Functions.getId recordType) patientId
                              ]

            SaveCompleted _ (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel ->
                { model | editData = Nothing } ! [ Functions.setUnsavedChanges False ]

            UpdateRecordAddNew sfData ->
                case model.editData of
                    Just editData ->
                        { model | editData = Just { editData | sfData = sfData } } ! []

                    Nothing ->
                        model ! [ displayErrorMessage "Cannot update edit data while null" ]

            UpdateTitle editData str ->
                updateAddNew { editData | title = str }

            UpdateSpecialty editData str ->
                updateAddNew { editData | specialty = str }

            UpdateProvider editData str ->
                updateAddNew { editData | provider = str }

            UpdateComments editData str ->
                updateAddNew { editData | comments = str }

            UpdateCallSid editData str ->
                updateAddNew { editData | callSid = str }

            UpdateRecordingSid editData str ->
                updateAddNew { editData | recording = str }

            UpdateDuration editData str ->
                updateAddNew { editData | duration = Functions.defaultIntStr str }

            -- Hospitilizations
            UpdateIsExistingHospitilization editData bool ->
                updateAddNew { editData | isExistingHospitilization = bool }

            UpdatePatientReported editData bool ->
                updateAddNew { editData | patientReported = bool }

            UpdateDischargeDiagnosis editData str ->
                updateAddNew { editData | dischargeDiagnosis = str }


getColumns : RecordType -> List (Table.Column RecordRow Msg)
getColumns recordType =
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
                    , hrefColumnExtra "Task" hrefCustom
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
            [ dropdownColumn (dropdownItems recordType)
            ]
    in
        List.append firstColumns lastColumns


hrefCustom : { a | taskId : Maybe Int, taskTitle : Maybe String } -> Html Msg
hrefCustom row =
    case ( row.taskId, row.taskTitle ) of
        ( Just t, Just y ) ->
            div [ class "RecordTableHref", onClick (EditTask t) ] [ text y ]

        _ ->
            div [] []


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


formInputs : EditData -> List (InputControlType Msg)
formInputs editData =
    let
        firstColumns =
            [ DropInput "Facility" Required editData.sfData.facilityId "FacilityId"
            ]

        lastControls =
            [ AreaInput "Comments" Required editData.comments (UpdateComments editData)
            , FileInput "Upload Record File" Required editData.sfData.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
                   , TextInput "Doctor of Visit" Optional editData.provider (UpdateProvider editData)
                   , TextInput "Specialty of Visit" Optional editData.specialty (UpdateSpecialty editData)
                   ]
                ++ lastControls

        columns =
            case editData.recordType of
                Common.PrimaryCare ->
                    defaultFields

                Common.Specialty ->
                    defaultFields

                Common.Labs ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString editData.sfData.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional editData.title (UpdateTitle editData)
                           , TextInput "Provider of Lab" Optional editData.provider (UpdateProvider editData)
                           ]
                        ++ lastControls

                Common.Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString editData.sfData.timeAcc) "TimeAccId"
                           , TextInput "Name of Study" Optional editData.title (UpdateTitle editData)
                           , TextInput "Provider of Study" Optional editData.provider (UpdateProvider editData)
                           ]
                        ++ lastControls

                Common.Misc ->
                    defaultFields

                Common.Legal ->
                    firstColumns
                        ++ TextInput "Title" Optional editData.title (UpdateTitle editData)
                        :: lastControls

                Common.Hospitalizations ->
                    case editData.isExistingHospitilization of
                        True ->
                            [ CheckInput "Existing Hospitilization"
                                Common.Optional
                                editData.isExistingHospitilization
                                (UpdateIsExistingHospitilization editData)
                            , DropInput "Select Hospitalization"
                                Common.Required
                                editData.sfData.hospitalizationId
                                "HospitalizationsId"
                            ]
                                ++ lastControls

                        False ->
                            [ CheckInput "Patient Reported" Common.Optional editData.patientReported (UpdatePatientReported editData)
                            , DropInputWithButton
                                "Facility"
                                Common.Optional
                                editData.sfData.facilityId
                                "FacilityId"
                                "Add New Facility"
                            , DateInput "Date of Admission" Required (defaultString editData.sfData.dateOfAdmission) "DateOfAdmissionId"
                            , DateInput "Date of Discharge" Required (defaultString editData.sfData.dateOfDischarge) "DateOfDischargeId"
                            , DropInput "Hospital Service Type" Required editData.sfData.hospitalServiceTypeId "HospitalServiceTypeId"
                            , AreaInput "Chief Complaint" Required editData.comments (UpdateComments editData)
                            , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
                            , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
                            , TextInput "Discharge Recommendations" Required editData.dischargeDiagnosis (UpdateDischargeDiagnosis editData)
                            , DropInputWithButton "Discharge Physician"
                                Optional
                                editData.sfData.dischargePhysicianId
                                "DischargePhysicianId"
                                "New Provider"
                            , DropInputWithButton
                                "Secondary Facility Name"
                                Optional
                                editData.sfData.facilityId2
                                "FacilityId2"
                                "Add New Facility"
                            , DateInput "Secondary Date of Admission" Optional (defaultString editData.sfData.dateOfAdmission) "DateOfAdmissionId2"
                            , DateInput "Secondary Date of Discharge" Optional (defaultString editData.sfData.dateOfDischarge) "DateOfDischargeId2"
                            , FileInput "Upload Record File" Required editData.sfData.fileName
                            ]
                                ++ lastControls

                Common.CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required editData.callSid (UpdateCallSid editData)
                           , TextInput "Recording Sid" Required editData.recording (UpdateRecordingSid editData)
                           , NumrInput "Duration" Required editData.duration (UpdateDuration editData)
                           , DateInput "Recording Date" Required (defaultString editData.sfData.recordingDate) "RecordingDateId"
                           , DropInput "User" Required editData.sfData.userId "UserId"
                           , DropInput "Task" Optional editData.sfData.taskId "TaskId"
                           ]

                Common.PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString editData.sfData.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required editData.sfData.fileName
                           ]

                Common.Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional editData.title (UpdateTitle editData)
                        :: lastControls
    in
        columns


gridConfig : RecordType -> Maybe AddEditDataSource -> Table.Config RecordRow Msg
gridConfig recordType addEditDataSource =
    { domTableId = "RecordTable"
    , toolbar =
        case addEditDataSource of
            Just t ->
                [ ( "e-addnew", Add t recordType ) ]

            Nothing ->
                [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns = getColumns recordType
    }


dropdownItems : RecordType -> List ( String, String, Int -> Msg )
dropdownItems recordType =
    case recordType of
        Common.CallRecordings ->
            [ ( "e-edit", "Mark As Consent", SendMenuMessage recordType "MarkAsConsent" ) ]

        _ ->
            [ ( "e-sync", "Transfer", SendMenuMessage recordType "Transfer" )
            , ( "e-download", "View File", SendMenuMessage recordType "ViewFile" )
            , ( "e-mail", "Send By Email", SendMenuMessage recordType "SendByEmail" )
            , ( "e-print_01", "Send By Fax", SendMenuMessage recordType "SendByFax" )
            , ( "e-save", "Save To Client Portal", SendMenuMessage recordType "SaveToClientPortal" )
            , ( "e-contextdelete", "Delete", SendMenuMessage recordType "Delete" )
            ]


encodeRecord : EditData -> Int -> Encode.Value
encodeRecord editData patientId =
    Encode.object
        [ ( "RecordId", maybeVal Encode.int <| editData.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", Encode.string <| editData.title )
        , ( "RecordTypeId", Encode.int <| Functions.getId editData.recordType )
        , ( "Specialty", Encode.string <| editData.specialty )
        , ( "Provider", Encode.string <| editData.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| editData.sfData.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| editData.sfData.timeAcc )
        , ( "RecordFile", Encode.string <| editData.sfData.fileName )
        , ( "Comments", Encode.string <| editData.comments )
        , ( "FacilityId", maybeVal Encode.int <| editData.sfData.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| editData.sfData.reportDate )
        , ( "CallSid", Encode.string <| editData.callSid )
        , ( "RecordingSid", Encode.string <| editData.recording )
        , ( "RecordingDuration", Encode.int <| editData.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| editData.sfData.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| editData.sfData.userId )
        , ( "TaskId", maybeVal Encode.int <| editData.sfData.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| editData.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| editData.sfData.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| editData.sfData.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| editData.sfData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| editData.sfData.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| editData.sfData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| editData.sfData.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| editData.sfData.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| editData.dischargeDiagnosis )
        , ( "DischargePhysicianId", maybeVal Encode.int <| editData.sfData.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| editData.sfData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| editData.sfData.dischargeDiagnosisId )
        ]


emptyModel : Model
emptyModel =
    { rows = []
    , dropDownState = -1
    , tableState = Table.init "Date"
    , addEditDataSource = Nothing
    , editData = Nothing
    }


createEditData : AddEditDataSource -> RecordType -> EditData
createEditData addEditDataSource recordType =
    { recordType = recordType
    , isExistingHospitilization = False
    , patientReported = False
    , dischargeDiagnosis = ""

    -- Hospitilizations
    , showValidationErrors = False
    , recordId = Nothing
    , title = ""
    , specialty = ""
    , provider = ""
    , comments = ""
    , recording = ""
    , callSid = ""
    , duration = 0
    , sfData = createSfData addEditDataSource recordType
    }


createSfData : AddEditDataSource -> RecordType -> SfData
createSfData addEditDataSource _ =
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
