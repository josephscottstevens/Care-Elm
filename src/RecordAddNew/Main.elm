port module RecordAddNew.Main exposing (..)

import RecordAddNew.Functions exposing (..)
import RecordAddNew.Types exposing (..)
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Route exposing (Route)
import Ports exposing (..)


port initRecords : InitRecordAddNew -> Cmd msg


port updateHospitilization : (DropDownItem -> msg) -> Sub msg


port updateFacility2 : (DropDownItem -> msg) -> Sub msg


port updateDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateDateOfAdmission2 : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge2 : (Maybe String -> msg) -> Sub msg


port updateHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateAdmitDiagnosis : (Maybe Int -> msg) -> Sub msg


port updateDischargeDiagnosis : (Maybe Int -> msg) -> Sub msg


port updateDischargePhysician : (DropDownItem -> msg) -> Sub msg


port presetPage : Maybe Int -> Cmd msg


port presetPageComplete : (Maybe Int -> msg) -> Sub msg


port resetUpdate : Maybe Int -> Cmd msg


port resetUpdateComplete : (Maybe Int -> msg) -> Sub msg


init : Maybe AddEditDataSource -> RecordType -> Cmd Msg
init addEditDataSource recordType =
    initRecords (getAddEditMsg addEditDataSource (Just <| getId recordType) False False)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ presetPageComplete PresetPageComplete
        , updateFacility UpdateFacility
        , updateCategory UpdateRecordType
        , updateTimeVisit UpdateTimeVisit
        , updateTimeAcc UpdateTimeAcc
        , updateFileName UpdateFileName
        , updateReportDate UpdateReportDate
        , updateRecordingDate UpdateRecordingDate
        , updateUser UpdateUser
        , updateTask UpdateTask

        -- Hospitilizations
        , updateHospitilization UpdateHospitilization
        , updateFacility2 UpdateFacility2
        , updateDateOfAdmission UpdateDateOfAdmission
        , updateDateOfDischarge UpdateDateOfDischarge
        , updateHospitalServiceType UpdateHospitalServiceType
        , updateAdmitDiagnosis UpdateAdmitDiagnosis
        , updateDischargeDiagnosis UpdateDischargeDiagnosis
        , updateDischargePhysician UpdateDischargePhysician
        , updateDateOfAdmission2 UpdateDateOfAdmission2
        , updateDateOfDischarge2 UpdateDateOfDischarge2
        ]


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

                saveBtnClass =
                    class "btn btn-sm btn-success margin-left-5 pull-right"
            in
                div [ class "form-horizontal" ]
                    [ h4 [] [ text (getDesc model.recordType) ]
                    , validationErrorsDiv
                    , makeControls defaultConfig (formInputs model model.recordType)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", id "Save", value "Addmodel", onClick (Save model.recordType), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick (Cancel model.recordType), class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
                            ]
                        ]
                    ]

        Limbo ->
            div [] []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                    model ! [ saveForm model, setUnsavedChanges False, Route.modifyUrl (Route.Records recordType) ]

            SaveCompleted (Ok responseMsg) ->
                case getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!" ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel recordType ->
                model ! [ setUnsavedChanges False, Route.modifyUrl (Route.Records recordType) ]

            PresetPageComplete recordTypeId ->
                { model | state = Edit } ! [ initRecords (getAddEditMsg model.addEditDataSource recordTypeId True False) ]

            UpdateRecordType dropDownItem ->
                if model.recordTypeId == dropDownItem.id then
                    model ! []
                else
                    case getRecordTypeById dropDownItem.id of
                        Just t ->
                            { model
                                | recordTypeId = dropDownItem.id
                                , recordType = t
                                , state = Limbo
                            }
                                ! [ presetPage dropDownItem.id ]

                        Nothing ->
                            model ! [ displayErrorMessage ("Cannot load invalid record type: " ++ toString dropDownItem.id) ]

            UpdateTitle str ->
                updateAddNew { model | title = str }

            UpdateSpecialty str ->
                updateAddNew { model | specialty = str }

            UpdateProvider str ->
                updateAddNew { model | provider = str }

            UpdateTimeVisit str ->
                updateAddNew { model | timeVisit = str }

            UpdateTimeAcc str ->
                updateAddNew { model | timeAcc = str }

            UpdateFileName str ->
                updateAddNew { model | fileName = str }

            UpdateComments str ->
                updateAddNew { model | comments = str }

            UpdateFacility dropDownItem ->
                updateAddNew { model | facilityId = dropDownItem.id, facilityText = dropDownItem.name }

            UpdateReportDate str ->
                updateAddNew { model | reportDate = str }

            UpdateCallSid str ->
                updateAddNew { model | callSid = str }

            UpdateRecordingSid str ->
                updateAddNew { model | recording = str }

            UpdateDuration str ->
                updateAddNew { model | duration = defaultIntStr str }

            UpdateRecordingDate str ->
                updateAddNew { model | recordingDate = str }

            UpdateUser dropDownItem ->
                updateAddNew { model | userId = dropDownItem.id, userText = dropDownItem.name }

            UpdateTask dropDownItem ->
                updateAddNew { model | taskId = dropDownItem.id, taskText = dropDownItem.name }

            -- Hospitilizations
            UpdateIsExistingHospitilization bool ->
                if model.isExistingHospitilization == bool then
                    model ! []
                else
                    { model | isExistingHospitilization = bool, state = Limbo } ! [ presetPage model.recordTypeId, setLoadingStatus True ]

            UpdateHospitilization dropDownItem ->
                updateAddNew { model | hospitalizationId = dropDownItem.id, hospitalizationText = dropDownItem.name }

            UpdatePatientReported bool ->
                updateAddNew { model | patientReported = bool }

            UpdateFacility2 dropDownItem ->
                updateAddNew { model | facilityId2 = dropDownItem.id, facilityText2 = dropDownItem.name }

            UpdateDateOfAdmission str ->
                updateAddNew { model | dateOfAdmission = str }

            UpdateDateOfDischarge str ->
                updateAddNew { model | dateOfDischarge = str }

            UpdateDateOfAdmission2 str ->
                updateAddNew { model | dateOfAdmission2 = str }

            UpdateDateOfDischarge2 str ->
                updateAddNew { model | dateOfDischarge2 = str }

            UpdateHospitalServiceType dropDownItem ->
                updateAddNew { model | hospitalServiceTypeId = dropDownItem.id, hospitalServiceTypeText = dropDownItem.name }

            UpdateAdmitDiagnosis admitDiagnosisId ->
                updateAddNew { model | admitDiagnosisId = admitDiagnosisId }

            UpdateDischargeDiagnosis dischargeDiagnosisId ->
                updateAddNew { model | dischargeDiagnosisId = dischargeDiagnosisId }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }

            UpdateDischargePhysician dropDownItem ->
                updateAddNew { model | dischargePhysicianId = dropDownItem.id, dischargePhysicianText = dropDownItem.name }


formInputs : Model -> RecordType -> List (InputControlType Msg)
formInputs model recordType =
    let
        firstColumns =
            [ DropInput "Facility" Required model.facilityId "FacilityId"
            , DropInput "Category" Required model.recordTypeId "CategoryId"
            ]

        lastColumns =
            [ AreaInput "Comments" Required model.comments UpdateComments
            , FileInput "Upload Record File" Required model.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString model.timeVisit) "TimeVisitId"
                   , TextInput "Doctor of Visit" Optional model.provider UpdateProvider
                   , TextInput "Specialty of Visit" Optional model.specialty UpdateSpecialty
                   ]
                ++ lastColumns

        columns =
            case recordType of
                PrimaryCare ->
                    defaultFields

                Specialty ->
                    defaultFields

                Labs ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString model.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString model.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional model.title UpdateTitle
                           , TextInput "Provider of Lab" Optional model.provider UpdateProvider
                           ]
                        ++ lastColumns

                Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString model.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString model.timeAcc) "TimeAccId"
                           , TextInput "Name of Study" Optional model.title UpdateTitle
                           , TextInput "Provider of Study" Optional model.provider UpdateProvider
                           ]
                        ++ lastColumns

                Misc ->
                    defaultFields

                Legal ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastColumns

                Hospitalizations ->
                    CheckInput "Existing Hospitilization" Optional model.isExistingHospitilization UpdateIsExistingHospitilization
                        :: case model.isExistingHospitilization of
                            True ->
                                (DropInput "Select Hospitalization" Required model.hospitalizationId "HospitalizationsId")
                                    :: lastColumns

                            False ->
                                [ CheckInput "Patient Reported" Optional model.patientReported UpdatePatientReported
                                , DropInputWithButton "Facility" Optional model.facilityId "FacilityId" "Add New Facility"
                                , DropInput "Category" Required model.recordTypeId "CategoryId"
                                , DateInput "Date of Admission" Required (defaultString model.dateOfAdmission) "DateOfAdmissionId"
                                , DateInput "Date of Discharge" Required (defaultString model.dateOfDischarge) "DateOfDischargeId"
                                , DropInput "Hospital Service Type" Required model.hospitalServiceTypeId "HospitalServiceTypeId"
                                , AreaInput "Chief Complaint" Required model.comments UpdateComments
                                , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
                                , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
                                , TextInput "Discharge Recommendations" Required model.dischargeRecommendations UpdateDischargeRecommendations
                                , DropInputWithButton "Discharge Physician" Optional model.dischargePhysicianId "DischargePhysicianId" "New Provider"
                                , DropInputWithButton "Secondary Facility Name" Optional model.facilityId2 "FacilityId2" "Add New Facility"
                                , DateInput "Secondary Date of Admission" Optional (defaultString model.dateOfAdmission) "DateOfAdmissionId2"
                                , DateInput "Secondary Date of Discharge" Optional (defaultString model.dateOfDischarge) "DateOfDischargeId2"
                                , FileInput "Upload Record File" Required model.fileName
                                ]

                CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required model.callSid UpdateCallSid
                           , TextInput "Recording Sid" Required model.recording UpdateRecordingSid
                           , NumrInput "Duration" Required model.duration UpdateDuration
                           , DateInput "Recording Date" Required (defaultString model.recordingDate) "RecordingDateId"
                           , DropInput "User" Required model.userId "UserId"
                           , DropInput "Task" Optional model.taskId "TaskId"
                           ]

                PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString model.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required model.fileName
                           ]

                Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastColumns
    in
        columns
