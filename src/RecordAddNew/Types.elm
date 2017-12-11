module RecordAddNew.Types exposing (..)

import Common.Types exposing (AddEditDataSource, RecordType, DropDownItem)
import Common.Functions as Functions
import Http


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


type State
    = Edit
    | Limbo


type alias Model =
    { state : State
    , addEditDataSource : AddEditDataSource
    , recordAddNewInitData : RecordAddNewInitData
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
    , dischargeRecommendations : String
    }


emptyModel : RecordType -> AddEditDataSource -> Model
emptyModel recordType addEditDataSource =
    { state = Edit
    , addEditDataSource = addEditDataSource
    , recordAddNewInitData = getAddEditMsg addEditDataSource recordType False False
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
    , dischargeRecommendations = ""
    }


type alias RecordAddNewInitData =
    { facilityId : Maybe Int
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , categoryId : Maybe Int
    , categoryText : String
    , users : List DropDownItem
    , tasks : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
    , hospitilizations : List DropDownItem
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
