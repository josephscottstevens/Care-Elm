module RecordAddNew.Types exposing (..)

import Common.Types exposing (..)
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
    , patientId : Int
    , recordTypeId : Maybe Int
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


emptyModel : RecordType -> AddEditDataSource -> Int -> Model
emptyModel recordType addEditDataSource patientId =
    { state = Edit
    , addEditDataSource = addEditDataSource
    , recordAddNewInitData = getAddEditMsg addEditDataSource recordType False False
    , recordType = recordType
    , recordId = 0
    , title = ""
    , patientId = patientId
    , recordTypeId = Just (getId recordType)
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
