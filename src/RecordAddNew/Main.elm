port module RecordAddNew.Main exposing (Msg, subscriptions, init, update, view)

import RecordAddNew.Functions exposing (saveForm)
import RecordAddNew.Types exposing (State(Edit, Limbo), Model, RecordAddNewInitData, getAddEditMsg)
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
import Common.Types as Common exposing (RequiredType(Required, Optional))
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, defaultString)
import Common.Ports exposing (setUnsavedChanges)
import Common.Route as Route
import Http


port presetPage : Maybe Int -> Cmd msg


port presetPageComplete : (Maybe Int -> msg) -> Sub msg


port initRecordAddNew : RecordAddNewInitData -> Cmd msg


port updateRecordAddNew : (RecordAddNewInitData -> msg) -> Sub msg


port updateCategory : (Common.DropdownItem -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ presetPageComplete PresetPageComplete
        , updateCategory UpdateRecordType
        , updateRecordAddNew UpdateRecordAddNew
        ]


init : Common.AddEditDataSource -> Common.RecordType -> Cmd Msg
init addEditDataSource recordType =
    initRecordAddNew (getAddEditMsg addEditDataSource recordType False False)


type Msg
    = AddNewFacility
    | AddNewPhysician
    | Save Common.RecordType
    | SaveCompleted (Result Http.Error String)
    | Cancel Common.RecordType
    | PresetPageComplete (Maybe Int)
    | UpdateRecordAddNew RecordAddNewInitData
    | UpdateTitle String
    | UpdateRecordType Common.DropdownItem
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

            UpdateRecordType dropdownItem ->
                if model.newRecord.categoryId == dropdownItem.id then
                    model ! []
                else
                    case Functions.getRecordTypeById dropdownItem.id of
                        Just t ->
                            { model | recordType = t, state = Limbo }
                                ! [ presetPage dropdownItem.id ]

                        Nothing ->
                            model ! [ displayErrorMessage ("Cannot load invalid record type: " ++ toString dropdownItem.id) ]

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
            in
                div [ class "form-horizontal" ]
                    [ h4 [] [ text (Functions.getDesc model.recordType) ]
                    , validationErrorsDiv
                    , makeControls defaultConfig (formInputs model model.recordType)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save model.recordType), class "btn btn-sm btn-success" ] [ text "Save" ]
                            , button [ type_ "button", onClick (Cancel model.recordType), class "btn btn-sm btn-default margin-left-5" ] [ text "Cancel" ]
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

        lastColumns =
            [ AreaInput "Comments" Required model.comments UpdateComments
            , FileInput "Upload Record File" Required model.newRecord.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString model.newRecord.timeVisit) "TimeVisitId"
                   , TextInput "Doctor of Visit" Optional model.provider UpdateProvider
                   , TextInput "Specialty of Visit" Optional model.specialty UpdateSpecialty
                   ]
                ++ lastColumns

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
                        ++ lastColumns

                Common.Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString model.newRecord.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString model.newRecord.timeAcc) "TimeAccId"
                           , TextInput "Name of Study" Optional model.title UpdateTitle
                           , TextInput "Provider of Study" Optional model.provider UpdateProvider
                           ]
                        ++ lastColumns

                Common.Misc ->
                    defaultFields

                Common.Legal ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastColumns

                Common.Hospitalizations ->
                    case model.isExistingHospitilization of
                        True ->
                            [ CheckInput "Existing Hospitilization" Common.Optional model.isExistingHospitilization UpdateIsExistingHospitilization
                            , DropInput "Select Hospitalization" Common.Required model.newRecord.hospitalizationId "HospitalizationsId"
                            ]
                                ++ lastColumns

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
                                ++ lastColumns

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
                        :: lastColumns
    in
        columns
