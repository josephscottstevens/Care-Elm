port module RecordAddNew.Main exposing (..)

import RecordAddNew.Functions exposing (..)
import RecordAddNew.Types exposing (..)
import Html exposing (Html, text, div, button, h4)
import Html.Attributes exposing (class, id, value, type_)
import Html.Events exposing (onClick)
import Common.Html exposing (..)
import Common.Types exposing (..)
import Common.Functions exposing (..)
import Common.Routes exposing (navRecords)
import Ports exposing (..)


port initRecords : InitRecordAddNew -> Cmd msg


port updateHospitilization : (DropDownItem -> msg) -> Sub msg


port updateFacility2 : (DropDownItem -> msg) -> Sub msg


port updateDateOfAdmission : (Maybe String -> msg) -> Sub msg


port updateDateOfDischarge : (Maybe String -> msg) -> Sub msg


port updateHospitalServiceType : (DropDownItem -> msg) -> Sub msg


port updateDischargePhysician : (DropDownItem -> msg) -> Sub msg


init : Maybe AddEditDataSource -> RecordType -> Cmd Msg
init addEditDataSource recordType =
    initRecords (getAddEditMsg addEditDataSource (getId recordType) False False)


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ updateFacility UpdateFacility
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
        , updateDischargePhysician UpdateDischargePhysician
        ]


view : Model -> RecordType -> Html Msg
view model recordType =
    let
        errors =
            getValidationErrors (formInputs model recordType)

        validationErrorsDiv =
            if model.showValidationErrors == True && List.length errors > 0 then
                div [ class "error margin-bottom-10" ] (List.map (\t -> div [] [ text t ]) errors)
            else
                div [] []

        saveBtnClass =
            class "btn btn-sm btn-success margin-left-5 pull-right"
    in
        div [ class "form-horizontal" ]
            [ h4 [] [ text (getDesc recordType) ]
            , validationErrorsDiv
            , makeControls (formInputs model recordType)
            , div [ class "form-group" ]
                [ div [ class fullWidth ]
                    [ button [ type_ "button", id "Save", value "Addmodel", onClick (Save recordType), saveBtnClass ] [ text "Save" ]
                    , button [ type_ "button", onClick (Cancel recordType), class "btn btn-sm btn-default pull-right" ] [ text "Cancel" ]
                    ]
                ]
            ]


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
                    model ! [ saveForm model, setUnsavedChanges False, navRecords recordType ]

            SaveCompleted (Ok responseMsg) ->
                case getResponseError responseMsg of
                    Just t ->
                        model ! [ displayErrorMessage t ]

                    Nothing ->
                        model ! [ displaySuccessMessage "Save completed successfully!" ]

            SaveCompleted (Err t) ->
                (model ! [ displayErrorMessage (toString t) ])

            Cancel recordType ->
                (model ! [ setUnsavedChanges False, navRecords recordType ])

            UpdateRecordType dropDownItem ->
                if model.recordTypeId == dropDownItem.id then
                    model ! []
                else
                    { model | recordTypeId = dropDownItem.id }
                        ! [ resetUpdate dropDownItem.id, setLoadingStatus True ]

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
                    { model | isExistingHospitilization = bool } ! [ resetUpdate model.recordTypeId, setLoadingStatus True ]

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

            UpdateHospitalServiceType dropDownItem ->
                updateAddNew { model | hospitalServiceTypeId = dropDownItem.id, hospitalServiceTypeText = dropDownItem.name }

            UpdateDischargeRecommendations str ->
                updateAddNew { model | dischargeRecommendations = str }

            UpdateDischargePhysician dropDownItem ->
                updateAddNew { model | dischargePhysicianId = dropDownItem.id, dischargePhysicianText = dropDownItem.name }


formInputs : Model -> RecordType -> List ( String, RequiredType, InputControlType Msg )
formInputs model recordType =
    let
        firstColumns =
            [ ( "Facility", Required, DropInput model.facilityId "FacilityId" )
            , ( "Category", Required, DropInput model.recordTypeId "CategoryId" )
            ]

        lastColumns =
            [ ( "Comments", Required, AreaInput model.comments UpdateComments )
            , ( "Upload Record File", Required, FileInput model.fileName )
            ]

        defaultFields =
            firstColumns
                ++ [ ( "Date of Visit", Required, DateInput (defaultString model.timeVisit) "TimeVisitId" UpdateTimeVisit )
                   , ( "Doctor of Visit", Optional, TextInput model.provider UpdateProvider )
                   , ( "Specialty of Visit", Optional, TextInput model.specialty UpdateSpecialty )
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
                        ++ [ ( "Date/Time of Labs Collected", Required, DateInput (defaultString model.timeVisit) "TimeVisitId" UpdateTimeVisit )
                           , ( "Date/Time of Labs Accessioned", Required, DateInput (defaultString model.timeAcc) "TimeAccId" UpdateTimeAcc )
                           , ( "Name of Lab", Optional, TextInput model.title UpdateTitle )
                           , ( "Provider of Lab", Optional, TextInput model.provider UpdateProvider )
                           ]
                        ++ lastColumns

                Radiology ->
                    firstColumns
                        ++ [ ( "Date/Time of Study was done", Required, DateInput (defaultString model.timeVisit) "TimeVisitId" UpdateTimeVisit )
                           , ( "Date/Time of Study Accessioned", Required, DateInput (defaultString model.timeAcc) "TimeAccId" UpdateTimeAcc )
                           , ( "Name of Study", Optional, TextInput model.title UpdateTitle )
                           , ( "Provider of Study", Optional, TextInput model.provider UpdateProvider )
                           ]
                        ++ lastColumns

                Misc ->
                    defaultFields

                Legal ->
                    firstColumns
                        ++ ( "Title", Optional, TextInput model.title UpdateTitle )
                        :: lastColumns

                Hospitalizations ->
                    ( "Existing Hospitilization", Optional, CheckInput model.isExistingHospitilization UpdateIsExistingHospitilization )
                        :: case model.isExistingHospitilization of
                            True ->
                                ( "Select Hospitalization", Required, DropInput model.hospitalizationId "HospitalizationsId" )
                                    :: lastColumns

                            False ->
                                [ ( "Patient Reported", Optional, CheckInput model.patientReported UpdatePatientReported )
                                , ( "Facility", Required, DropInputWithButton model.facilityId "FacilityId" AddNewFacility "Add New Facility" )
                                , ( "Category", Required, DropInput model.recordTypeId "CategoryId" )
                                , ( "Date of Admission", Required, DateInput (defaultString model.dateOfAdmission) "DateOfAdmissionId" UpdateDateOfAdmission )
                                , ( "Date of Discharge", Required, DateInput (defaultString model.dateOfDischarge) "DateOfDischargeId" UpdateDateOfDischarge )
                                , ( "Hospital Service Type", Required, DropInput model.hospitalServiceTypeId "HospitalServiceTypeId" )
                                , ( "Chief Complaint", Required, AreaInput model.comments UpdateComments )
                                , ( "Admit Diagnosis", Required, KnockInput "HospitalizationAdmitProblemSelection" )
                                , ( "Discharge Diagnosis", Required, KnockInput "HospitalizationDischargeProblemSelection" )
                                , ( "Discharge Recommendations", Required, TextInput model.dischargeRecommendations UpdateDischargeRecommendations )
                                , ( "Discharge Physician", Required, DropInputWithButton model.dischargePhysicianId "DischargePhysicianId" AddNewPhysician "New Provider" )
                                , ( "Secondary Facility Name", Required, DropInputWithButton model.facilityId2 "FacilityId2" AddNewFacility "Add New Facility" )
                                , ( "Secondary Date of Admission", Required, DateInput (defaultString model.dateOfAdmission) "DateOfAdmissionId2" UpdateDateOfAdmission )
                                , ( "Secondary Date of Discharge", Required, DateInput (defaultString model.dateOfDischarge) "DateOfDischargeId2" UpdateDateOfDischarge )
                                ]

                CallRecordings ->
                    firstColumns
                        ++ [ ( "Call Sid", Required, TextInput model.callSid UpdateCallSid )
                           , ( "Recording Sid", Required, TextInput model.recording UpdateRecordingSid )
                           , ( "Duration", Required, NumrInput model.duration UpdateDuration )
                           , ( "Recording Date", Required, DateInput (defaultString model.recordingDate) "RecordingDateId" UpdateRecordingDate )
                           , ( "User", Required, DropInput model.userId "UserId" )
                           , ( "Task", Optional, DropInput model.taskId "TaskId" )
                           ]

                PreviousHistories ->
                    firstColumns
                        ++ [ ( "Report Date", Required, DateInput (defaultString model.reportDate) "ReportDateId" UpdateReportDate )
                           , ( "Upload Record File", Required, FileInput model.fileName )
                           ]

                Enrollment ->
                    firstColumns
                        ++ ( "Title", Optional, TextInput model.title UpdateTitle )
                        :: lastColumns
    in
        columns
