module RecordAddNew.Types exposing (State(Edit, Limbo), Model, RecordAddNewInitData, getAddEditMsg, emptyModel)

import Common.Types exposing (AddEditDataSource, RecordType, DropdownItem)
import Common.Functions as Functions


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
