port module Records exposing (Model, Msg, emptyModel, init, subscriptions, update, view)

import Common.Functions as Functions exposing (defaultString, displayErrorMessage, displaySuccessMessage, maybeVal, sendMenuMessage)
import Common.Html
    exposing
        ( InputControlType(AreaInput, CheckInput, DateInput, DropInput, DropInputWithButton, FileInput, KnockInput, NumrInput, TextInput)
        , defaultConfig
        , fullWidth
        , getValidationErrors
        , makeControls
        )
import Common.Table as Table
import Common.Types as Common exposing (AddEditDataSource, DropdownItem, RecordType(..), RequiredType(Optional, Required))
import Html exposing (Html, button, div, h4, text)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onClick)
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
        [ Functions.dialogConfirmed DeleteConfirmed
        , updateRecordAddNew UpdateRecordAddNew
        ]


init : Common.RecordType -> Int -> Cmd Msg
init recordType patientId =
    loadRecords recordType patientId


type alias Model =
    { rows : List Row
    , dropDownState : Int
    , recordType : RecordType
    , tableState : Table.State
    , editData : Maybe EditData
    }


type alias Row =
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
    , dischargeDiagnosis : Maybe String

    -- Hospitilizations
    , showValidationErrors : Bool
    , recordId : Maybe Int
    , title : Maybe String
    , specialty : Maybe String
    , provider : Maybe String
    , comments : Maybe String
    , recording : Maybe String
    , callSid : Maybe String
    , duration : Int
    , sfData : SfData
    }


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    let
        config =
            gridConfig model.recordType addEditDataSource
    in
        div []
            [ h4 [] [ text (Functions.getDesc model.recordType) ]
            , case model.editData of
                Just editData ->
                    Table.view model.tableState model.rows config (Just <| viewEditData editData)

                Nothing ->
                    Table.view model.tableState model.rows config Nothing
            ]


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
    = Load (Result Http.Error (List Row))
    | SetTableState Table.State
    | Add AddEditDataSource RecordType
    | SendMenuMessage Common.RecordType String Row
    | EditTask Int
    | DeletePrompt Row
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
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

            SendMenuMessage recordType messageType row ->
                { model | rows = flipConsent model.rows row.id recordType }
                    ! [ sendMenuMessage (getMenuMessage model.rows recordType row.id messageType) ]

            DeletePrompt row ->
                model ! [ Functions.deleteDialogShow row.id ]

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
                              , loadRecords model.recordType patientId
                              ]

            SaveCompleted _ (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            Cancel ->
                { model | editData = Nothing } ! [ Functions.setUnsavedChanges False ]

            UpdateRecordAddNew sfData ->
                case model.editData of
                    Just editData ->
                        { model | editData = Just { editData | sfData = sfData } } ! []

                    Nothing ->
                        model ! [ displayErrorMessage "Cannot update edit data while null" ]

            UpdateTitle editData str ->
                updateAddNew { editData | title = Just str }

            UpdateSpecialty editData str ->
                updateAddNew { editData | specialty = Just str }

            UpdateProvider editData str ->
                updateAddNew { editData | provider = Just str }

            UpdateComments editData str ->
                updateAddNew { editData | comments = Just str }

            UpdateCallSid editData str ->
                updateAddNew { editData | callSid = Just str }

            UpdateRecordingSid editData str ->
                updateAddNew { editData | recording = Just str }

            UpdateDuration editData str ->
                updateAddNew { editData | duration = Functions.defaultIntStr str }

            -- Hospitilizations
            UpdateIsExistingHospitilization editData bool ->
                updateAddNew { editData | isExistingHospitilization = bool }

            UpdatePatientReported editData bool ->
                updateAddNew { editData | patientReported = bool }

            UpdateDischargeDiagnosis editData str ->
                updateAddNew { editData | dischargeDiagnosis = Just str }


getColumns : RecordType -> List (Table.Column Row Msg)
getColumns recordType =
    let
        commonColumns =
            [ Table.dateTimeColumn "Date Collected" .date
            , Table.stringColumn "Doctor of Visit" .provider
            , Table.stringColumn "Specialty" .specialty
            , Table.htmlColumn "Comments" .comments
            ]

        firstColumns =
            case recordType of
                Common.PrimaryCare ->
                    commonColumns

                Common.Specialty ->
                    commonColumns

                Common.Labs ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.dateTimeColumn "Date Accessioned" .dateAccessed
                    , Table.stringColumn "Name of Lab" .title
                    , Table.stringColumn "Provider" .provider
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.Radiology ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.dateTimeColumn "Date Accessioned" .dateAccessed
                    , Table.stringColumn "Name of Study" .title
                    , Table.stringColumn "Provider" .provider
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.Hospitalizations ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.intColumn "Hospitalization ID" .hospitalizationId
                    , Table.dateTimeColumn "Admin Collected" .dateOfAdmission
                    , Table.dateTimeColumn "Discharge Date" .dateOfDischarge
                    , Table.stringColumn "Service Type" .hospitalizationServiceType
                    , Table.stringColumn "Discharge Recommendations" .recommendations
                    , Table.stringColumn "Discharge Physician" .dischargePhysician
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.Legal ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.CallRecordings ->
                    [ Table.dateColumn "Date" .recordingDate
                    , Table.hrefColumn "Recording" "Open" .recording
                    , Table.hrefColumnExtra "Task" hrefCustom
                    , Table.checkColumn "During Enrollment" .enrollment
                    , Table.checkColumn "Consent" .hasVerbalConsent
                    , Table.stringColumn "User" .staffName
                    ]

                Common.PreviousHistories ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.stringColumn "File Name" .fileName
                    , Table.dateColumn "Report Date" .reportDate
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.Enrollment ->
                    [ Table.dateTimeColumn "Date Collected" .date
                    , Table.htmlColumn "Comments" .comments
                    ]

                Common.Misc ->
                    commonColumns

                Common.ContinuityOfCareDocument ->
                    [ Table.dateTimeColumn "Date" .date
                    ]

        lastColumns =
            [ Table.dropdownColumn (dropdownItems recordType)
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


decodeRow : Decoder Row
decodeRow =
    decode Row
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


loadRecords : Common.RecordType -> Int -> Cmd Msg
loadRecords recordType patientId =
    let
        recordTypeId =
            Functions.getId recordType

        url =
            "/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId
    in
        Decode.field "list" (Decode.list decodeRow)
            |> Http.get url
            |> Http.send Load


getMenuMessage : List Row -> Common.RecordType -> Int -> String -> Common.MenuMessage
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


flipConsent : List Row -> Int -> Common.RecordType -> List Row
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
        firstColumn =
            DropInput "Facility" Required editData.sfData.facilityId "FacilityId"

        lastControls =
            [ AreaInput "Comments" Required editData.comments (UpdateComments editData)
            , FileInput "Upload Record File" Required editData.sfData.fileName
            ]

        defaultFields =
            [ firstColumn
            , DateInput "Date of Visit" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
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
                    [ firstColumn
                    , DateInput "Date of Labs Collected" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
                    , DateInput "Date of Labs Accessioned" Required (defaultString editData.sfData.timeAcc) "TimeAccId"
                    , TextInput "Name of Lab" Optional editData.title (UpdateTitle editData)
                    , TextInput "Provider of Lab" Optional editData.provider (UpdateProvider editData)
                    ]
                        ++ lastControls

                Common.Radiology ->
                    [ firstColumn
                    , DateInput "Date of Study was done" Required (defaultString editData.sfData.timeVisit) "TimeVisitId"
                    , DateInput "Date of Study Accessioned" Required (defaultString editData.sfData.timeAcc) "TimeAccId"
                    , TextInput "Name of Study" Optional editData.title (UpdateTitle editData)
                    , TextInput "Provider of Study" Optional editData.provider (UpdateProvider editData)
                    ]
                        ++ lastControls

                Common.Misc ->
                    defaultFields

                Common.Legal ->
                    [ firstColumn
                    , TextInput "Title" Optional editData.title (UpdateTitle editData)
                    ]
                        ++ lastControls

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
                    [ firstColumn
                    , TextInput "Call Sid" Required editData.callSid (UpdateCallSid editData)
                    , TextInput "Recording Sid" Required editData.recording (UpdateRecordingSid editData)
                    , NumrInput "Duration" Required editData.duration (UpdateDuration editData)
                    , DateInput "Recording Date" Required (defaultString editData.sfData.recordingDate) "RecordingDateId"
                    , DropInput "User" Required editData.sfData.userId "UserId"
                    , DropInput "Task" Optional editData.sfData.taskId "TaskId"
                    ]

                Common.PreviousHistories ->
                    [ firstColumn
                    , DateInput "Report Date" Required (defaultString editData.sfData.reportDate) "ReportDateId"
                    , FileInput "Upload Record File" Required editData.sfData.fileName
                    ]

                Common.Enrollment ->
                    [ firstColumn
                    , TextInput "Title" Optional editData.title (UpdateTitle editData)
                    ]
                        ++ lastControls

                Common.ContinuityOfCareDocument ->
                    [ firstColumn
                    , FileInput "Upload Record File" Required editData.sfData.fileName
                    ]
    in
        columns


gridConfig : RecordType -> Maybe AddEditDataSource -> Table.Config Row Msg
gridConfig recordType addEditDataSource =
    { domTableId = "RecordTable"
    , toolbar =
        case recordType of
            Common.ContinuityOfCareDocument ->
                []

            _ ->
                case addEditDataSource of
                    Just t ->
                        [ ( "e-addnew e-loaded", Add t recordType ) ]

                    Nothing ->
                        [ ( "e-addnew e-disable", NoOp ) ]
    , toMsg = SetTableState
    , columns = getColumns recordType
    , onDoubleClick =
        case recordType of
            Common.CallRecordings ->
                Nothing

            _ ->
                Just (SendMenuMessage recordType "ViewFile")
    }


dropdownItems : RecordType -> List ( String, String, Row -> Msg )
dropdownItems recordType =
    case recordType of
        Common.CallRecordings ->
            [ ( "e-edit", "Mark As Consent", SendMenuMessage recordType "MarkAsConsent" ) ]

        Common.ContinuityOfCareDocument ->
            [ ( "e-download", "View File", SendMenuMessage recordType "ViewFile" ) ]

        _ ->
            [ ( "e-sync", "Transfer", SendMenuMessage recordType "Transfer" )
            , ( "e-download", "View File", SendMenuMessage recordType "ViewFile" )
            , ( "e-mail", "Send By Email", SendMenuMessage recordType "SendByEmail" )
            , ( "e-print_01", "Send By Fax", SendMenuMessage recordType "SendByFax" )
            , ( "e-save", "Save To Client Portal", SendMenuMessage recordType "SaveToClientPortal" )
            , ( "e-contextdelete", "Delete", DeletePrompt )
            ]


encodeRecord : EditData -> Int -> Encode.Value
encodeRecord editData patientId =
    Encode.object
        [ ( "RecordId", maybeVal Encode.int <| editData.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", maybeVal Encode.string <| editData.title )
        , ( "RecordTypeId", Encode.int <| Functions.getId editData.recordType )
        , ( "Specialty", maybeVal Encode.string <| editData.specialty )
        , ( "Provider", maybeVal Encode.string <| editData.provider )
        , ( "TimeVisit", maybeVal Encode.string <| editData.sfData.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| editData.sfData.timeAcc )
        , ( "RecordFile", Encode.string <| editData.sfData.fileName )
        , ( "Comments", maybeVal Encode.string <| editData.comments )
        , ( "FacilityId", maybeVal Encode.int <| editData.sfData.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| editData.sfData.reportDate )
        , ( "CallSid", maybeVal Encode.string <| editData.callSid )
        , ( "RecordingSid", maybeVal Encode.string <| editData.recording )
        , ( "RecordingDuration", Encode.int <| editData.duration )
        , ( "RecordingDate", maybeVal Encode.string <| editData.sfData.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| editData.sfData.userId )
        , ( "TaskId", maybeVal Encode.int <| editData.sfData.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| editData.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| editData.sfData.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| editData.sfData.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| editData.sfData.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| editData.sfData.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| editData.sfData.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| editData.sfData.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| editData.sfData.hospitalServiceTypeId )
        , ( "DischargeRecommendations", maybeVal Encode.string <| editData.dischargeDiagnosis )
        , ( "DischargePhysicianId", maybeVal Encode.int <| editData.sfData.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| editData.sfData.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| editData.sfData.dischargeDiagnosisId )
        ]


emptyModel : RecordType -> Model
emptyModel recordType =
    let
        sortColumn =
            case recordType of
                CallRecordings ->
                    "Date"

                ContinuityOfCareDocument ->
                    "Date"

                _ ->
                    "Date Collected"
    in
        { rows = []
        , dropDownState = -1
        , tableState = Table.init sortColumn
        , editData = Nothing
        , recordType = recordType
        }


createEditData : AddEditDataSource -> RecordType -> EditData
createEditData addEditDataSource recordType =
    { recordType = recordType
    , isExistingHospitilization = False
    , patientReported = False
    , dischargeDiagnosis = Nothing

    -- Hospitilizations
    , showValidationErrors = False
    , recordId = Nothing
    , title = Nothing
    , specialty = Nothing
    , provider = Nothing
    , comments = Nothing
    , recording = Nothing
    , callSid = Nothing
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
