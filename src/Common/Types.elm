module Common.Types exposing (..)


type Page
    = None
    | Billing
    | Records RecordType
    | RecordAddNew RecordType
    | Hospitilizations
    | HospitilizationsAddEdit (Maybe Int)
    | Error String


type alias FilterState =
    { name : String
    , value : String
    }


type RequiredType
    = Required
    | Optional


type alias DropDownItem =
    { id : Maybe Int
    , name : String
    }


type alias DropDownState =
    { x : Float
    , y : Float
    , rowId : Int
    }


type alias MenuMessage =
    { name : String
    , recordId : Int
    , recordTypeId : Maybe Int
    , hasConsent : Maybe Bool
    }


type RecordType
    = PrimaryCare
    | Specialty
    | Labs
    | Radiology
    | Hospitalizations
    | Legal
    | CallRecordings
    | PreviousHistories
    | Enrollment
    | Misc


getDesc : RecordType -> String
getDesc recordType =
    case recordType of
        PrimaryCare ->
            "Primary Care Records"

        Specialty ->
            "Specialty Records"

        Labs ->
            "Lab Records"

        Radiology ->
            "Radiology Records"

        Hospitalizations ->
            "Hospitalization Records"

        Legal ->
            "Legal Records"

        CallRecordings ->
            "Call Recording Records"

        PreviousHistories ->
            "Previous History Records"

        Enrollment ->
            "Enrollment Records"

        Misc ->
            "Miscellaneous Records"


getId : RecordType -> Maybe Int
getId recordType =
    case recordType of
        PrimaryCare ->
            Just 1

        Specialty ->
            Just 2

        Labs ->
            Just 3

        Radiology ->
            Just 4

        Hospitalizations ->
            Just 9

        Legal ->
            Just 6

        CallRecordings ->
            Just 10

        PreviousHistories ->
            Just 11

        Enrollment ->
            Just 12

        Misc ->
            Just 5


type alias AddEditDataSource =
    { facilityId : Maybe Int
    , patientId : Int
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
    , hospitilizations : List DropDownItem
    }


type alias RecordAddNewInitData =
    { id : Maybe Int
    , patientId : Int
    , facilityId : Maybe Int
    , patientReported : Bool
    , hospitalizationId : Maybe Int
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , chiefComplaint : String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , facilityId2 : Maybe Int
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    }
