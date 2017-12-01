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
    | UpdateDateOfAdmission (Maybe String)
    | UpdateDateOfDischarge (Maybe String)
    | UpdateHospitalServiceType DropDownItem
    | UpdateChiefComplaint String
    | UpdateDischargeRecommendations String
    | UpdateDischargePhysician DropDownItem
    | UpdateFacility2 DropDownItem
    | UpdateDateOfAdmission2 (Maybe String)
    | UpdateDateOfDischarge2 (Maybe String)


type alias Model =
    { id : Maybe Int
    , patientId : Int
    , facilityId : Maybe Int
    , facilityText : String
    , patientReported : Bool
    , hospitalizationId : Maybe Int
    , hospitalizationText : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , chiefComplaint : String
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    , showValidationErrors : Bool
    }


emptyModel : Int -> Model
emptyModel patientId =
    { id = Nothing
    , patientId = patientId
    , facilityId = Nothing
    , facilityText = ""
    , patientReported = False
    , hospitalizationId = Nothing
    , hospitalizationText = ""
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , hospitalServiceTypeId = Nothing
    , hospitalServiceTypeText = ""
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , dischargePhysicianText = ""
    , facilityId2 = Nothing
    , facilityText2 = ""
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    , showValidationErrors = False
    }


type alias InitHospitilizationsAddNew =
    { facilityId : Maybe Int
    , facilities : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
    , hospitilizations : List DropDownItem
    , hospitilizationId : Maybe Int
    }
