port module RecordAddNew exposing (Msg, Model, subscriptions, init, update, view, emptyModel)

import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Html
    exposing
        ( getValidationErrors
        , defaultConfig
        , makeControls
        , fullWidth
        , InputControlType(DropInput, TextInput, AreaInput, DateInput, FileInput, DropInputWithButton, KnockInput, CheckInput, NumrInput)
        )
import Common.Types as Common exposing (RequiredType(Required, Optional), AddEditDataSource, RecordType, DropdownItem)
import Common.Functions as Functions exposing (maybeToDateString, setUnsavedChanges, maybeVal, displayErrorMessage, displaySuccessMessage, defaultString)
import Common.Route as Route
import Http
import Json.Encode as Encode


port presetPage : Maybe Int -> Cmd msg


port presetPageComplete : (Maybe Int -> msg) -> Sub msg


port initRecordAddNew : RecordAddNewInitData -> Cmd msg


port updateRecordAddNew : (RecordAddNewInitData -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ presetPageComplete PresetPageComplete
        , updateRecordAddNew UpdateRecordAddNew
        ]


init : Common.AddEditDataSource -> Common.RecordType -> Cmd Msg
init addEditDataSource recordType =
    initRecordAddNew (getAddEditMsg addEditDataSource recordType False False)


type State
    = Edit
    | Limbo


type alias Model =
    { state : State
    , addEditDataSource : AddEditDataSource
    , newRecord : RecordAddNewInitData
    , recordType : RecordType
    , recordId : Int
    , title : String
    , recordTypeText : String
    , specialty : String
    , provider : String
    , comments : String
    , showValidationErrors : Bool
    , recording : String
    , callSid : String
    , duration : Int

    -- Hospitilizations
    , isExistingHospitilization : Bool
    , patientReported : Bool
    , dischargeDiagnosis : String
    }


type Msg
    = AddNewFacility
    | AddNewPhysician
    | Save Common.RecordType
    | SaveCompleted (Result Http.Error String)
    | Cancel Common.RecordType
    | PresetPageComplete (Maybe Int)
    | UpdateRecordAddNew RecordAddNewInitData
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
            t ! [ setUnsavedChanges True ]
    in
        case msg of
            AddNewFacility ->
                model ! [ addNewFacility Nothing ]

            AddNewPhysician ->
                model ! [ addNewPhysician Nothing ]

            Save recordType ->
                if List.length (getValidationErrors (formInputs model recordType)) > 0 then
                    { model | showValidationErrors = True } ! []
                else
                    model ! [ saveForm model patientId SaveCompleted, setUnsavedChanges False ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t, Route.back ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!", Route.back ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel recordType ->
                model ! [ setUnsavedChanges False, Route.modifyUrl (Route.Records recordType) ]

            PresetPageComplete recordTypeId ->
                case Functions.getRecordTypeById recordTypeId of
                    Just t ->
                        { model | state = Edit } ! [ initRecordAddNew (getAddEditMsg model.addEditDataSource t True False) ]

                    Nothing ->
                        model ! [ Route.modifyUrl (Route.Records Common.PrimaryCare) ]

            UpdateRecordAddNew recordAddNew ->
                { model | newRecord = recordAddNew } ! []

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


view : Model -> Html Msg
view model =
    case model.state of
        Edit ->
            let
                errors =
                    getValidationErrors (formInputs model model.recordType)

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
                    , makeControls defaultConfig (formInputs model model.recordType)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save model.recordType), saveClass ] [ text "Save" ]
                            , button [ type_ "button", onClick (Cancel model.recordType), cancelClass ] [ text "Cancel" ]
                            ]
                        ]
                    ]

        Limbo ->
            div [] []


formInputs : Model -> Common.RecordType -> List (InputControlType Msg)
formInputs model recordType =
    let
        firstColumns =
            [ DropInput "Facility" Required model.newRecord.facilityId "FacilityId"
            , DropInput "Category" Required model.newRecord.categoryId "CategoryId"
            ]

        lastControls =
            [ AreaInput "Comments" Required model.comments UpdateComments
            , FileInput "Upload Record File" Required model.newRecord.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString model.newRecord.timeVisit) "TimeVisitId"
                   , TextInput "Doctor of Visit" Optional model.provider UpdateProvider
                   , TextInput "Specialty of Visit" Optional model.specialty UpdateSpecialty
                   ]
                ++ lastControls

        columns =
            case recordType of
                Common.PrimaryCare ->
                    defaultFields

                Common.Specialty ->
                    defaultFields

                Common.Labs ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString model.newRecord.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString model.newRecord.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional model.title UpdateTitle
                           , TextInput "Provider of Lab" Optional model.provider UpdateProvider
                           ]
                        ++ lastControls

                Common.Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString model.newRecord.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString model.newRecord.timeAcc) "TimeAccId"
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
                            , DropInput "Select Hospitalization" Common.Required model.newRecord.hospitalizationId "HospitalizationsId"
                            ]
                                ++ lastControls

                        False ->
                            [ CheckInput "Patient Reported" Common.Optional model.patientReported UpdatePatientReported
                            , DropInputWithButton
                                "Facility"
                                Common.Optional
                                model.newRecord.facilityId
                                "FacilityId"
                                "Add New Facility"
                            , DropInput "Category" Required model.newRecord.categoryId "CategoryId"
                            , DateInput "Date of Admission" Required (defaultString model.newRecord.dateOfAdmission) "DateOfAdmissionId"
                            , DateInput "Date of Discharge" Required (defaultString model.newRecord.dateOfDischarge) "DateOfDischargeId"
                            , DropInput "Hospital Service Type" Required model.newRecord.hospitalServiceTypeId "HospitalServiceTypeId"
                            , AreaInput "Chief Complaint" Required model.comments UpdateComments
                            , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
                            , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
                            , TextInput "Discharge Recommendations" Required model.dischargeDiagnosis UpdateDischargeDiagnosis
                            , DropInputWithButton "Discharge Physician"
                                Optional
                                model.newRecord.dischargePhysicianId
                                "DischargePhysicianId"
                                "New Provider"
                            , DropInputWithButton
                                "Secondary Facility Name"
                                Optional
                                model.newRecord.facilityId2
                                "FacilityId2"
                                "Add New Facility"
                            , DateInput "Secondary Date of Admission" Optional (defaultString model.newRecord.dateOfAdmission) "DateOfAdmissionId2"
                            , DateInput "Secondary Date of Discharge" Optional (defaultString model.newRecord.dateOfDischarge) "DateOfDischargeId2"
                            , FileInput "Upload Record File" Required model.newRecord.fileName
                            ]
                                ++ lastControls

                Common.CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required model.callSid UpdateCallSid
                           , TextInput "Recording Sid" Required model.recording UpdateRecordingSid
                           , NumrInput "Duration" Required model.duration UpdateDuration
                           , DateInput "Recording Date" Required (defaultString model.newRecord.recordingDate) "RecordingDateId"
                           , DropInput "User" Required model.newRecord.userId "UserId"
                           , DropInput "Task" Optional model.newRecord.taskId "TaskId"
                           ]

                Common.PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString model.newRecord.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required model.newRecord.fileName
                           ]

                Common.Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastControls
    in
        columns


encodeRecord : Model -> Int -> Encode.Value
encodeRecord newRecord patientId =
    Encode.object
        [ ( "RecordId", Encode.int <| newRecord.recordId )
        , ( "PatientId", Encode.int <| patientId )
        , ( "Title", Encode.string <| newRecord.title )
        , ( "RecordTypeId", Encode.int <| Functions.getId newRecord.recordType )
        , ( "Specialty", Encode.string <| newRecord.specialty )
        , ( "Provider", Encode.string <| newRecord.provider )
        , ( "TimeVisit", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.timeVisit )
        , ( "TimeAcc", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.timeAcc )
        , ( "RecordFile", Encode.string <| newRecord.newRecord.fileName )
        , ( "Comments", Encode.string <| newRecord.comments )
        , ( "FacilityId", maybeVal Encode.int <| newRecord.newRecord.facilityId )
        , ( "ReportDate", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.reportDate )
        , ( "CallSid", Encode.string <| newRecord.callSid )
        , ( "RecordingSid", Encode.string <| newRecord.recording )
        , ( "RecordingDuration", Encode.int <| newRecord.duration )
        , ( "RecordingDate", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.recordingDate )
        , ( "StaffId", maybeVal Encode.int <| newRecord.newRecord.userId )
        , ( "TaskId", maybeVal Encode.int <| newRecord.newRecord.taskId )

        -- Hospitilizations
        , ( "PatientReported", Encode.bool <| newRecord.patientReported )
        , ( "HospitalizationId", maybeVal Encode.int <| newRecord.newRecord.hospitalizationId )
        , ( "FacilityId2", maybeVal Encode.int <| newRecord.newRecord.facilityId2 )
        , ( "DateOfAdmission", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfAdmission )
        , ( "DateOfDischarge", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfDischarge )
        , ( "DateOfAdmission2", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfAdmission2 )
        , ( "DateOfDischarge2", maybeVal Encode.string <| maybeToDateString <| newRecord.newRecord.dateOfDischarge2 )
        , ( "HospitalServiceTypeId", maybeVal Encode.int <| newRecord.newRecord.hospitalServiceTypeId )
        , ( "DischargeRecommendations", Encode.string <| newRecord.dischargeDiagnosis )
        , ( "DischargePhysicianId", maybeVal Encode.int <| newRecord.newRecord.dischargePhysicianId )
        , ( "AdmitDiagnosisId", maybeVal Encode.int <| newRecord.newRecord.admitDiagnosisId )
        , ( "DischargeDiagnosisId", maybeVal Encode.int <| newRecord.newRecord.dischargeDiagnosisId )
        ]


saveFormRequest : Model -> Int -> Http.Request String
saveFormRequest model patientId =
    Http.request
        { body = encodeRecord model patientId |> Http.jsonBody
        , expect = Http.expectString
        , headers = []
        , method = "POST"
        , timeout = Nothing
        , url = "/People/AddNewRecord"
        , withCredentials = False
        }


saveForm : Model -> Int -> (Result Http.Error String -> msg) -> Cmd msg
saveForm model patientId saveCompleted =
    Http.send saveCompleted (saveFormRequest model patientId)


emptyModel : RecordType -> AddEditDataSource -> Model
emptyModel recordType addEditDataSource =
    { state = Edit
    , addEditDataSource = addEditDataSource
    , newRecord = getAddEditMsg addEditDataSource recordType False False
    , recordType = recordType
    , recordId = 0
    , title = ""
    , recordTypeText = ""
    , specialty = ""
    , provider = ""
    , comments = ""
    , showValidationErrors = False
    , recording = ""
    , callSid = ""
    , duration = 0

    -- Hospitilizations
    , isExistingHospitilization = False
    , patientReported = False
    , dischargeDiagnosis = ""
    }


type alias RecordAddNewInitData =
    { facilityId : Maybe Int
    , facilities : List DropdownItem
    , recordTypes : List DropdownItem
    , categoryId : Maybe Int
    , categoryText : String
    , users : List DropdownItem
    , tasks : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , hospitilizations : List DropdownItem
    , setFocus : Bool
    , isExistingHospitilization : Bool

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


getAddEditMsg : AddEditDataSource -> RecordType -> Bool -> Bool -> RecordAddNewInitData
getAddEditMsg addEditDataSource recordType setFocus isExistingHospitilization =
    { facilityId = addEditDataSource.facilityId
    , facilities = addEditDataSource.facilities
    , categoryId = Just (Functions.getId recordType)
    , categoryText = ""
    , recordTypes = addEditDataSource.recordTypes
    , users = addEditDataSource.users
    , tasks = addEditDataSource.tasks
    , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
    , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
    , hospitilizations = addEditDataSource.hospitilizations
    , setFocus = setFocus
    , isExistingHospitilization = isExistingHospitilization

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
