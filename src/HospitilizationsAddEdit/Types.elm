module HospitilizationsAddEdit.Types exposing (..)

import Common.Types exposing (..)
import Http


type Msg
    = Save
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateFacility DropDownItem
    | AddNewFacility
    | AddNewPhysician
    | UpdateIsExistingHospitilization Bool
    | UpdateHospitilization DropDownItem
    | UpdatePatientReported Bool
    | UpdateFacility2 DropDownItem
    | UpdateDateOfAdmission (Maybe String)
    | UpdateDateOfDischarge (Maybe String)
    | UpdateHospitalServiceType DropDownItem
    | UpdateChiefComplaint String
    | UpdateDischargeRecommendations String
    | UpdateDischargePhysician DropDownItem


type alias Model =
    { recordId : Int
    , title : String
    , patientId : Int
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

    -- Hospitilizations
    , isExistingHospitilization : Bool
    , patientReported : Bool
    , hospitalizationId : Maybe Int
    , hospitalizationText : String
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , chiefComplaint : String
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    }


emptyModel : Flags -> Model
emptyModel flags =
    { recordId = 0
    , title = ""
    , patientId = flags.patientId
    , recordTypeId = flags.recordTypeId
    , recordTypeText = ""
    , specialty = ""
    , provider = ""
    , timeVisit = Nothing
    , timeAcc = Nothing
    , fileName = ""
    , comments = ""
    , showValidationErrors = False
    , reportDate = Nothing
    , facilityId = Nothing
    , facilityText = ""
    , recording = ""
    , callSid = ""
    , duration = 0
    , recordingDate = Nothing
    , userId = Nothing
    , userText = ""
    , taskId = Nothing
    , taskText = ""

    -- Hospitilizations
    , isExistingHospitilization = False
    , patientReported = False
    , hospitalizationId = Nothing
    , hospitalizationText = ""
    , facilityId2 = Nothing
    , facilityText2 = ""
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , hospitalServiceTypeId = Nothing
    , hospitalServiceTypeText = ""
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , dischargePhysicianText = ""
    }
