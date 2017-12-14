module HospitilizationsAddEdit.Types exposing (Model, SyncfusionData, emptyHospitilizationRow, emptyModel)

import Common.Types exposing (HospitilizationsRow, DropdownItem)


type alias Model =
    { sfData : SyncfusionData
    , patientReported : Bool
    , chiefComplaint : String
    , dischargeRecommendations : String
    , showValidationErrors : Bool
    }


emptyModel : Model
emptyModel =
    { sfData = emptySfData
    , patientReported = False
    , chiefComplaint = ""
    , dischargeRecommendations = ""
    , showValidationErrors = False
    }


emptySfData : SyncfusionData
emptySfData =
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
    , dropdownOpen = False

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


type alias SyncfusionData =
    { id : Maybe Int
    , facilities : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , facilityId : Maybe Int
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , facilityId2 : Maybe Int
    , hospitalServiceTypeId : Maybe Int
    , dischargePhysicianId : Maybe Int
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    }
