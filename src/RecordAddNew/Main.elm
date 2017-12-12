port module RecordAddNew.Main exposing (Msg, subscriptions, init, update, view)

import RecordAddNew.Functions exposing (saveForm)
import RecordAddNew.Types exposing (State(Edit, Limbo), Model, RecordAddNewInitData, getAddEditMsg)
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (getValidationErrors, defaultConfig, makeControls, fullWidth, InputControlType(..))
import Common.Types exposing (RecordType(..), DropDownItem, RequiredType(..), AddEditDataSource)
import Common.Functions as Functions exposing (displayErrorMessage, displaySuccessMessage, defaultString)
import Common.Ports exposing (setUnsavedChanges)
import Route
import Http


port presetPage : Maybe Int -> Cmd msg


port presetPageComplete : (Maybe Int -> msg) -> Sub msg


port initRecordAddNew : RecordAddNewInitData -> Cmd msg


port updateRecordAddNew : (RecordAddNewInitData -> msg) -> Sub msg


port updateCategory : (DropDownItem -> msg) -> Sub msg


port addNewFacility : Maybe String -> Cmd msg


port addNewPhysician : Maybe String -> Cmd msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ presetPageComplete PresetPageComplete
        , updateCategory UpdateRecordType
        , updateRecordAddNew UpdateRecordAddNew
        ]


init : AddEditDataSource -> RecordType -> Cmd Msg
init addEditDataSource recordType =
    initRecordAddNew (getAddEditMsg addEditDataSource recordType False False)


type Msg
    = AddNewFacility
    | AddNewPhysician
    | Save RecordType
    | SaveCompleted (Result Http.Error String)
    | Cancel RecordType
    | PresetPageComplete (Maybe Int)
    | UpdateRecordAddNew RecordAddNewInitData
    | UpdateTitle String
    | UpdateRecordType DropDownItem
    | UpdateSpecialty String
    | UpdateProvider String
    | UpdateComments String
    | UpdateCallSid String
    | UpdateRecordingSid String
    | UpdateDuration String
      -- Hospitilizations
    | UpdateIsExistingHospitilization Bool
    | UpdatePatientReported Bool
    | UpdateDischargeRecommendations String


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
                    model ! [ saveForm model patientId SaveCompleted, setUnsavedChanges False, Route.modifyUrl (Route.Records recordType) ]

            SaveCompleted (Ok responseMsg) ->
                case Functions.getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!" ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel recordType ->
                model ! [ setUnsavedChanges False, Route.modifyUrl (Route.Records recordType) ]

            PresetPageComplete recordTypeId ->
                case Functions.getRecordTypeById recordTypeId of
                    Just t ->
                        { model | state = Edit } ! [ initRecordAddNew (getAddEditMsg model.addEditDataSource t True False) ]

                    Nothing ->
                        model ! [ Route.modifyUrl (Route.Records PrimaryCare) ]

            UpdateRecordAddNew recordAddNew ->
                { model | recordAddNewInitData = recordAddNew } ! []

            UpdateRecordType dropDownItem ->
                if model.recordAddNewInitData.categoryId == dropDownItem.id then
                    model ! []
                else
                    case Functions.getRecordTypeById dropDownItem.id of
                        Just t ->
                            { model | recordType = t, state = Limbo }
                                ! [ presetPage dropDownItem.id ]

                        Nothing ->
                            model ! [ displayErrorMessage ("Cannot load invalid record type: " ++ toString dropDownItem.id) ]

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

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }


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

                cancelBtnClass =
                    class "btn btn-sm btn-default pull-right"
            in
                div [ class "form-horizontal" ]
                    [ h4 [] [ text (Functions.getDesc model.recordType) ]
                    , validationErrorsDiv
                    , makeControls defaultConfig (formInputs model model.recordType)
                    , div [ class "form-group" ]
                        [ div [ class fullWidth ]
                            [ button [ type_ "button", onClick (Save model.recordType), saveBtnClass ] [ text "Save" ]
                            , button [ type_ "button", onClick (Cancel model.recordType), cancelBtnClass ] [ text "Cancel" ]
                            ]
                        ]
                    ]

        Limbo ->
            div [] []


formInputs : Model -> RecordType -> List (InputControlType Msg)
formInputs model recordType =
    let
        firstColumns =
            [ DropInput "Facility" Required model.recordAddNewInitData.facilityId "FacilityId"
            , DropInput "Category" Required model.recordAddNewInitData.categoryId "CategoryId"
            ]

        lastColumns =
            [ AreaInput "Comments" Required model.comments UpdateComments
            , FileInput "Upload Record File" Required model.recordAddNewInitData.fileName
            ]

        defaultFields =
            firstColumns
                ++ [ DateInput "Date of Visit" Required (defaultString model.recordAddNewInitData.timeVisit) "TimeVisitId"
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
                        ++ [ DateInput "Date/Time of Labs Collected" Required (defaultString model.recordAddNewInitData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Labs Accessioned" Required (defaultString model.recordAddNewInitData.timeAcc) "TimeAccId"
                           , TextInput "Name of Lab" Optional model.title UpdateTitle
                           , TextInput "Provider of Lab" Optional model.provider UpdateProvider
                           ]
                        ++ lastColumns

                Radiology ->
                    firstColumns
                        ++ [ DateInput "Date/Time of Study was done" Required (defaultString model.recordAddNewInitData.timeVisit) "TimeVisitId"
                           , DateInput "Date/Time of Study Accessioned" Required (defaultString model.recordAddNewInitData.timeAcc) "TimeAccId"
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
                                DropInput "Select Hospitalization" Required model.recordAddNewInitData.hospitalizationId "HospitalizationsId"
                                    :: lastColumns

                            False ->
                                [ CheckInput "Patient Reported" Optional model.patientReported UpdatePatientReported
                                , DropInputWithButton "Facility" Optional model.recordAddNewInitData.facilityId "FacilityId" "Add New Facility"
                                , DropInput "Category" Required model.recordAddNewInitData.categoryId "CategoryId"
                                , DateInput "Date of Admission" Required (defaultString model.recordAddNewInitData.dateOfAdmission) "DateOfAdmissionId"
                                , DateInput "Date of Discharge" Required (defaultString model.recordAddNewInitData.dateOfDischarge) "DateOfDischargeId"
                                , DropInput "Hospital Service Type" Required model.recordAddNewInitData.hospitalServiceTypeId "HospitalServiceTypeId"
                                , AreaInput "Chief Complaint" Required model.comments UpdateComments
                                , KnockInput "Admit Diagnosis" Required "HospitalizationAdmitProblemSelection"
                                , KnockInput "Discharge Diagnosis" Required "HospitalizationDischargeProblemSelection"
                                , TextInput "Discharge Recommendations" Required model.dischargeRecommendations UpdateDischargeRecommendations
                                , DropInputWithButton "Discharge Physician" Optional model.recordAddNewInitData.dischargePhysicianId "DischargePhysicianId" "New Provider"
                                , DropInputWithButton "Secondary Facility Name" Optional model.recordAddNewInitData.facilityId2 "FacilityId2" "Add New Facility"
                                , DateInput "Secondary Date of Admission" Optional (defaultString model.recordAddNewInitData.dateOfAdmission) "DateOfAdmissionId2"
                                , DateInput "Secondary Date of Discharge" Optional (defaultString model.recordAddNewInitData.dateOfDischarge) "DateOfDischargeId2"
                                , FileInput "Upload Record File" Required model.recordAddNewInitData.fileName
                                ]

                CallRecordings ->
                    firstColumns
                        ++ [ TextInput "Call Sid" Required model.callSid UpdateCallSid
                           , TextInput "Recording Sid" Required model.recording UpdateRecordingSid
                           , NumrInput "Duration" Required model.duration UpdateDuration
                           , DateInput "Recording Date" Required (defaultString model.recordAddNewInitData.recordingDate) "RecordingDateId"
                           , DropInput "User" Required model.recordAddNewInitData.userId "UserId"
                           , DropInput "Task" Optional model.recordAddNewInitData.taskId "TaskId"
                           ]

                PreviousHistories ->
                    firstColumns
                        ++ [ DateInput "Report Date" Required (defaultString model.recordAddNewInitData.reportDate) "ReportDateId"
                           , FileInput "Upload Record File" Required model.recordAddNewInitData.fileName
                           ]

                Enrollment ->
                    firstColumns
                        ++ TextInput "Title" Optional model.title UpdateTitle
                        :: lastColumns
    in
        columns
