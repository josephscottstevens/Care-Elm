module HospitilizationsAddEdit.Types exposing (..)

import Common.Types exposing (HospitilizationsInitData, AddEditDataSource, HospitilizationsRow)
import Http


type alias Model =
    { initData : HospitilizationsInitData
    , patientReported : Bool
    , chiefComplaint : String
    , dischargeRecommendations : String
    , showValidationErrors : Bool
    }


emptyModel : HospitilizationsInitData -> Model
emptyModel hospitilizationsInitData =
    { initData = hospitilizationsInitData
    , patientReported = False
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , showValidationErrors = False
    }


type alias HospitilizationsData =
    { addEditDataSource : Maybe AddEditDataSource
    , hospitilizationsRow : HospitilizationsRow
    }


initData : HospitilizationsInitData
initData =
    { id = Nothing
    , facilities = []
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
