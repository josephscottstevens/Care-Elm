module HospitilizationsAddEdit.Types exposing (..)

import Common.Types exposing (..)
import Http


type Msg
    = Save
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateHospitilizationsInitData HospitilizationsInitData
    | UpdatePatientReported Bool
    | UpdateChiefComplaint String
    | UpdateDischargeRecommendations String


type alias Model =
    { initData : HospitilizationsInitData
    , patientId : Int
    , patientReported : Bool
    , chiefComplaint : String
    , dischargeRecommendations : String
    , showValidationErrors : Bool
    }


emptyModel : Int -> Model
emptyModel patientId =
    { patientId = patientId
    , initData = emptyHospitilizationsInitData
    , patientReported = False
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , showValidationErrors = False
    }


type alias HospitilizationsData =
    { addEditDataSource : Maybe AddEditDataSource
    , hospitilizationsRow : HospitilizationsRow
    }


emptyHospitilizationsInitData : HospitilizationsInitData
emptyHospitilizationsInitData =
    { facilities = []
    , hospitilizationServiceTypes = []
    , hospitalizationDischargePhysicians = []
    , facilityId = Nothing
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , facilityId2 = Nothing
    , hospitalServiceTypeId = Nothing
    , dischargePhysicianId = Nothing
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    }


emptyHospitilizationRow : HospitilizationsRow
emptyHospitilizationRow =
    { id = 0
    , facilityName = Nothing
    , dateOfAdmission = Nothing
    , admitProblem = Nothing
    , dateOfDischarge = Nothing
    , dischargeProblem = Nothing
    , serviceType = Nothing
    , fromTcm = False
    , recordId = Nothing
    , dropDownOpen = False

    -- for edit
    , patientId = 0
    , facilityId = Nothing
    , patientReported = False
    , hospitalizationId = Nothing
    , hospitalServiceTypeId = Nothing
    , chiefComplaint = ""
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , dischargeRecommendations = ""
    , dischargePhysicianId = Nothing
    , facilityId2 = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    }
