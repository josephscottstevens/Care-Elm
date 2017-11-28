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
    { id : Maybe Int
    , patientId : Int
    , facilityId : Maybe Int
    , facilityText : String
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
    , showValidationErrors : Bool
    }


emptyModel : Flags -> Model
emptyModel flags =
    { id = Nothing
    , patientId = flags.patientId
    , facilityId = Nothing
    , facilityText = ""
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
    , showValidationErrors = False
    }
