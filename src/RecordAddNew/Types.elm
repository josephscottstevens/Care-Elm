module RecordAddNew.Types exposing (..)

import Http
import Utils.CommonTypes exposing (..)


type Msg
    = -- SaveCompleted (Result Http.Error String)
      -- | Save
      -- | ResetAddNew String
      AddNewFacility
    | AddNewPhysician
    | UpdateTitle String
    | UpdateRecordType DropDownItem
    | UpdateSpecialty String
    | UpdateProvider String
    | UpdateTimeVisit (Maybe String)
    | UpdateTimeAcc (Maybe String)
    | UpdateFileName String
    | UpdateComments String
    | UpdateFacility DropDownItem
    | UpdateReportDate (Maybe String)
    | UpdateCallSid String
    | UpdateRecordingSid String
    | UpdateDuration String
    | UpdateRecordingDate (Maybe String)
    | UpdateUser DropDownItem
    | UpdateTask DropDownItem
      -- Hospitilizations
    | UpdateFacility2 DropDownItem
    | UpdateDateOfAdmission (Maybe String)
    | UpdateDateOfDischarge (Maybe String)
    | UpdateHospitalServiceType DropDownItem
    | UpdateDischargeRecommendations String
    | UpdateDischargePhysician DropDownItem


type ModelState
    = AddEdit
    | Limbo
    | Error String


type alias Model =
    { state : ModelState
    , recordId : Int
    , title : String
    , recordTypeId : Maybe Int
    , recordTypeText : String
    , specialty : String
    , provider : String
    , timeVisit : Maybe String
    , timeAcc : Maybe String
    , fileName : String
    , comments : String
    , showValidationErrors : Bool
    , reportDate : Maybe String
    , facilityId : Maybe Int
    , facilityText : String
    , recording : String
    , callSid : String
    , duration : Int
    , recordingDate : Maybe String
    , userId : Maybe Int
    , userText : String
    , taskId : Maybe Int
    , taskText : String

    -- dropdowns
    -- , addEditDataSource : AddEditDataSource
    -- Hospitilizations
    , hospitalizationId : Maybe Int
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    }


emptyModel : Model
emptyModel =
    { state = AddEdit
    , recordId = 0
    , facilityId = Nothing
    , title = ""
    , recordTypeId = Nothing
    , recordTypeText = ""
    , specialty = ""
    , provider = ""
    , timeVisit = Nothing
    , timeAcc = Nothing
    , fileName = ""
    , comments = ""
    , showValidationErrors = False
    , reportDate = Nothing
    , facilityText = ""
    , recording = ""
    , callSid = ""
    , duration = 0
    , recordingDate = Nothing
    , userId = Nothing
    , userText = ""
    , taskId = Nothing
    , taskText = ""

    -- dropdowns
    -- , addEditDataSource = dataSource
    -- Hospitilizations
    , hospitalizationId = Nothing
    , facilityId2 = Nothing
    , facilityText2 = ""
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , hospitalServiceTypeId = Nothing
    , hospitalServiceTypeText = ""
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , dischargePhysicianText = ""
    }
