module HospitilizationsAddEdit.Types exposing (..)

import Common.Types exposing (..)
import Http


type Msg
    = Save
    | SaveCompleted (Result Http.Error String)
    | Cancel
    | UpdateHospitilizationsInitData HospitilizationsInitData
    | UpdatePatientReported Bool
    | UpdateDateOfAdmission (Maybe String)
    | UpdateDateOfDischarge (Maybe String)
    | UpdateChiefComplaint String
    | UpdateDischargeRecommendations String
    | UpdateDateOfAdmission2 (Maybe String)
    | UpdateDateOfDischarge2 (Maybe String)


type alias Model =
    { id : Maybe Int
    , initData : HospitilizationsInitData
    , patientId : Int
    , patientReported : Bool
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , chiefComplaint : String
    , dischargeRecommendations : String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    , showValidationErrors : Bool
    }


emptyModel : Int -> Model
emptyModel patientId =
    { id = Nothing
    , patientId = patientId
    , patientReported = False
    , initData = emptyHospitilizationsInitData
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
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
    , patientId = -47
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

    -- Our control
    , chiefComplaint = ""
    , dischargeRecommendations = ""
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
