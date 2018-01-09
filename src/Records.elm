port module Records exposing (Msg, Model, emptyModel, subscriptions, init, update, view)

import Html exposing (Html, text, div, h4, button)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Table as Table exposing (defaultCustomizations)
import Common.Grid exposing (hrefColumn, checkColumn)
import Common.Types as Common exposing (RequiredType(Required, Optional), AddEditDataSource, RecordType, DropdownItem)
import Common.Functions as Functions exposing (sendMenuMessage, displaySuccessMessage, displayErrorMessage, maybeVal, defaultString, maybeToDateString)
import Common.Route as Route
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
import Json.Decode.Pipeline exposing (decode, required, hardcoded)
import Json.Encode as Encode


port presetPage : Maybe Int -> Cmd msg


port presetPageComplete : (Maybe Int -> msg) -> Sub msg


port initRecordAddNew : EditData -> Cmd msg


port updateRecordAddNew : (EditData -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


port editTask : Int -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Functions.deleteConfirmed DeleteConfirmed
        , presetPageComplete PresetPageComplete
        , updateRecordAddNew UpdateRecordAddNew
        ]


type State
    = Edit
    | Limbo
    | Grid


init : Common.RecordType -> Int -> Cmd Msg
init recordType patientId =
    getRecords recordType patientId
        |> Http.send Load


type alias Model =
    { rows : List RecordRow
    , tableState : Table.State
    , dropDownState : Int
    , state : State
    , recordType : RecordType

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
    , recordingDate : String
    , recordingDuration : Int
    , enrollment : Bool
    , staffId : Int
    , staffName : Maybe String
    , hasVerbalConsent : Bool
    , dropdownOpen : Bool
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


view : Model -> Maybe AddEditDataSource -> Html Msg
view model addEditDataSource =
    case model.state of
        Grid ->
            div []
                [ h4 [] [ text (Functions.getDesc model.recordType) ]
                , div [ class "e-grid e-js e-waitingpopup" ]
                    [ Table.view (config addEditDataSource model.recordType model.tableState) model.tableState model.rows ]
                ]

        Edit ->
            case model.editData of
                Just editData ->
                    let
                        errors =
                            getValidationErrors (formInputs model editData)

                        validationErrorsDiv =
                            if model.showValidationErrors == True && List.length errors > 0 then
                                div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
                            else
                                div [] []

                        saveClass =
                            class "btn btn-sm btn-success"

                        cancelClass =
                            class "btn btn-sm btn-default margin-left-5"
                    in
                        div [ class "form-horizontal" ]
                            [ h4 [] [ text (Functions.getDesc model.recordType) ]
                            , validationErrorsDiv
                            , makeControls defaultConfig (formInputs model editData)
                            , div [ class "form-group" ]
                                [ div [ class fullWidth ]
                                    [ button [ type_ "button", onClick (Save editData), saveClass ] [ text "Save" ]
                                    , button [ type_ "button", onClick Cancel, cancelClass ] [ text "Cancel" ]
                                    ]
                                ]
                            ]

                Nothing ->
                    div [] []

        Limbo ->
            div [] []


type Msg
    = Load (Result Http.Error (List RecordRow))
    | SetTableState Table.State
    | Add
    | SendMenuMessage Int Common.RecordType String
    | EditTask Int
    | DeletePrompt Int
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
      -- Edit Messages
    | AddNewFacility
    | AddNewPhysician
    | Save EditData
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | PresetPageComplete (Maybe Int)
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


update : Msg -> Model -> Int -> ( Model, Cmd Msg )
update msg model patientId =
    let
        updateAddNew t =
            t ! [ Functions.setUnsavedChanges True ]
    in
        case msg of
            Load (Ok t) ->
                { model | rows = t } ! [ Functions.setLoadingStatus False ]

            Load (Err t) ->
                model ! [ displayErrorMessage (toString t) ]

            Add ->
                { model | state = Limbo } ! [ presetPage (Just (Functions.getId model.recordType)) ]

            SetTableState newState ->
                { model | tableState = newState } ! []

            SendMenuMessage recordId recordType messageType ->
                { model | rows = flipConsent model.rows recordId recordType }
                    ! [ sendMenuMessage (getMenuMessage model.rows recordType recordId messageType) ]

            DeletePrompt rowId ->
                model ! [ Functions.deletePrompt rowId ]

            DeleteConfirmed rowId ->
                let
                    updatedRecords =
                        model.rows |> List.filter (\t -> t.id /= rowId)
                in
                    { model | rows = updatedRecords } ! [ deleteRequest rowId DeleteCompleted ]

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

            -- Edit
            AddNewFacility ->
                model ! [ addNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addNewPhysician Nothing ]

            Save editData ->
                if List.length (getValidationErrors (formInputs model editData)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    model
                        ! [ "/People/AddNewRecord"
                                |> Functions.postRequest (encodeRecord model editData patientId)
                                |> Http.send SaveCompleted
                          , Functions.setUnsavedChanges False
                          ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t, Route.back ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!", Route.back ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel ->
                { model | state = Grid } ! [ Functions.setUnsavedChanges False ]

            PresetPageComplete _ ->
                case model.addEditDataSource of
                    Just t ->
                        { model | state = Edit } ! [ initRecordAddNew (getEditData t model.recordType) ]

                    Nothing ->
                        model ! [ displayErrorMessage "invalid add edit datasource" ]

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
                if model.isExistingHospitilization == bool then
                    model ! []
                else
                    { model | isExistingHospitilization = bool, state = Limbo }
                        ! [ presetPage (Just (Functions.getId model.recordType)), Functions.setLoadingStatus True ]

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateDischargeDiagnosis str ->
                updateAddNew { model | dischargeDiagnosis = str }


getColumns : Common.RecordType -> Table.State -> List (Table.Column RecordRow Msg)
getColumns recordType state =
    let
        dropDownItems rowId =
            case recordType of
                Common.CallRecordings ->
                    [ ( "e-edit", "Mark As Consent", onClick (SendMenuMessage rowId recordType "MarkAsConsent") ) ]

                _ ->
                    [ ( "e-sync", "Transfer", onClick (SendMenuMessage rowId recordType "Transfer") )
                    , ( "e-download", "View File", onClick (SendMenuMessage rowId recordType "ViewFile") )
                    , ( "e-mail", "Send By Email", onClick (SendMenuMessage rowId recordType "SendByEmail") )
                    , ( "e-print_01", "Send By Fax", onClick (SendMenuMessage rowId recordType "SendByFax") )
                    , ( "e-save", "Save To Client Portal", onClick (SendMenuMessage rowId recordType "SaveToClientPortal") )
                    , ( "e-contextdelete", "Delete", onClick (DeletePrompt rowId) )
                    ]

        commonColumns =
            [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
            , Table.stringColumn "Doctor of Visit" (\t -> Functions.defaultString t.provider)
            , Table.stringColumn "Specialty" (\t -> Functions.defaultString t.specialty)
            , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
            ]

        firstColumns =
            case recordType of
                Common.PrimaryCare ->
                    commonColumns

                Common.Specialty ->
                    commonColumns

                Common.Labs ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , Table.stringColumn "Name of Lab" (\t -> Functions.defaultString t.title)
                    , Table.stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Radiology ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "Date Accessioned" (\t -> Functions.defaultDateTime t.dateAccessed)
                    , Table.stringColumn "Name of Study" (\t -> Functions.defaultString t.title)
                    , Table.stringColumn "Provider" (\t -> Functions.defaultString t.provider)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Hospitalizations ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "Hospitalization ID" (\t -> Functions.defaultIntToString t.hospitalizationId)
                    , Table.stringColumn "Admin Date" (\t -> Functions.defaultDateTime t.dateOfAdmission)
                    , Table.stringColumn "Discharge Date" (\t -> Functions.defaultDateTime t.dateOfDischarge)
                    , Table.stringColumn "Service Type" (\t -> Functions.defaultString t.hospitalizationServiceType)
                    , Table.stringColumn "Discharge Recommendations" (\t -> Functions.defaultString t.recommendations)
                    , Table.stringColumn "Discharge Physician" (\t -> Functions.defaultString t.dischargePhysician)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Legal ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.CallRecordings ->
                    [ Table.stringColumn "Date" (\t -> Functions.dateTime t.recordingDate)
                    , hrefColumn "Recording" "Open" (\t -> Functions.defaultString t.recording)
                    , hrefCustom
                    , checkColumn "During Enrollment" (\t -> t.enrollment)
                    , checkColumn "Consent" (\t -> t.hasVerbalConsent)
                    , Table.stringColumn "User" (\t -> Functions.defaultString t.staffName)
                    ]

                Common.PreviousHistories ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "File Name" (\t -> Functions.defaultString t.fileName)
                    , Table.stringColumn "Report Date" (\t -> Functions.defaultDate t.reportDate)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Enrollment ->
                    [ Table.stringColumn "Date Collected" (\t -> Functions.defaultDateTime t.date)
                    , Table.stringColumn "Comments" (\t -> Functions.defaultString t.comments)
                    ]

                Common.Misc ->
                    commonColumns

        lastColumns =
            [ Table.dropdownColumn (\t -> Table.dropdownDetails (dropDownItems t.id) t.id state SetTableState)
            ]
    in
        List.append firstColumns lastColumns


config : Maybe Common.AddEditDataSource -> Common.RecordType -> Table.State -> Table.Config RecordRow Msg
config addEditDataSource recordType state =
    let
        buttons =
            case addEditDataSource of
                Just _ ->
                    [ ( "e-addnew", onClick Add ) ]

                Nothing ->
                    []
    in
        Table.customConfig
            { toId = \t -> toString t.id
            , toMsg = SetTableState
            , columns = getColumns recordType state
            , customizations =
                { defaultCustomizations
                    | tableAttrs = Common.Grid.standardTableAttrs "RecordTable"
                    , thead = Common.Grid.standardTheadNoFilters
                    , theadButtons = buttons
                }
            }


hrefCustom : Table.Column RecordRow Msg
hrefCustom =
    Table.veryCustomColumn
        { name = "Task"
        , viewData = \t -> hrefCustomDetails t.taskId t.taskTitle
        , sorter = Table.unsortable
        }


hrefCustomDetails : Maybe Int -> Maybe String -> Table.HtmlDetails Msg
hrefCustomDetails taskId taskTitle =
    Table.HtmlDetails []
        [ case ( taskId, taskTitle ) of
            ( Just t, Just y ) ->
                div [ class "RecordTableHref", onClick (EditTask t) ] [ text y ]

            _ ->
                div [] []
        ]


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
        |> required "RecordingDate" Decode.string
        |> required "RecordingDuration" Decode.int
        |> required "Enrollment" Decode.bool
        |> required "StaffId" Decode.int
        |> required "StaffName" (maybe Decode.string)
        |> required "HasVerbalConsent" Decode.bool
        |> hardcoded False


getRecords : Common.RecordType -> Int -> Http.Request (List RecordRow)
getRecords recordType patientId =
    let
        recordTypeId =
            Functions.getId recordType

        url =
            "/People/PatientRecordsGrid?patientId=" ++ toString patientId ++ "&recordTypeId=" ++ toString recordTypeId
    in
        Decode.field "list" (Decode.list decodeRecordRow)
            |> Http.get url


deleteRequest : a -> (Result Http.Error String -> msg) -> Cmd msg
deleteRequest rowId deleteCompleted =
    Http.send deleteCompleted <| Http.getString ("/People/DeleteRecord?recordId=" ++ toString rowId)


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


formInputs : Model -> EditData -> List (InputControlType Msg)
formInputs model editData =
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
            case model.recordType of
                Common.PrimaryCare ->
                    defaultFields

                Common.Specialty ->
                    defaultFields

                Common.Labs ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString editData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString editData.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional model.title UpdateTitle
                           , TextInput "Provider of Lab" Optional model.provider UpdateProvider
                           ]
                        ++ lastControls

                Common.Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString editData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString editData.timeAcc) "TimeAccId"
                           , TextInput "Name of Study" Optional model.title UpdateTitle
                           , TextInput "Provider of Study" Optional model.provider UpdateProvider
                           ]
                        ++ lastControls

                Common.Misc ->
                    defaultFields

                Common.Legal ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastControls

                Common.Hospitalizations ->
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

                Common.CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required model.callSid UpdateCallSid
                           , TextInput "Recording Sid" Required model.recording UpdateRecordingSid
                           , NumrInput "Duration" Required model.duration UpdateDuration
                           , DateInput "Recording Date" Required (defaultString editData.recordingDate) "RecordingDateId"
                           , DropInput "User" Required editData.userId "UserId"
                           , DropInput "Task" Optional editData.taskId "TaskId"
                           ]

                Common.PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString editData.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required editData.fileName
                           ]

                Common.Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastControls
    in
        columns


encodeRecord : Model -> EditData -> Int -> Encode.Value
encodeRecord newRecord editData patientId =
    Encode.object
        [ ( "RecordId", maybeVal Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", Encode.int <| Functions.getId newRecord.recordType )
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


emptyModel : RecordType -> Maybe AddEditDataSource -> Model
emptyModel recordType addEditDataSource =
    { state = Grid
    , addEditDataSource = addEditDataSource
    , recordType = recordType
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
    , tableState = Table.initialSort "Date"
    , dropDownState = -1
    }


getEditData : AddEditDataSource -> RecordType -> EditData
getEditData addEditDataSource recordType =
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
