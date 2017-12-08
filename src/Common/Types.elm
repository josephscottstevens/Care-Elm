module Common.Types exposing (..)


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


getRecordTypeById : Maybe Int -> Maybe RecordType
getRecordTypeById maybeInt =
    case maybeInt of
        Just int ->
            case int of
                1 ->
                    Just PrimaryCare

                2 ->
                    Just Specialty

                3 ->
                    Just Labs

                4 ->
                    Just Radiology

                9 ->
                    Just Hospitalizations

                6 ->
                    Just Legal

                10 ->
                    Just CallRecordings

                11 ->
                    Just PreviousHistories

                12 ->
                    Just Enrollment

                5 ->
                    Just Misc

                _ ->
                    Nothing

        Nothing ->
            Nothing


getId : RecordType -> Int
getId recordType =
    case recordType of
        PrimaryCare ->
            1

        Specialty ->
            2

        Labs ->
            3

        Radiology ->
            4

        Hospitalizations ->
            9

        Legal ->
            6

        CallRecordings ->
            10

        PreviousHistories ->
            11

        Enrollment ->
            12

        Misc ->
            5


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


type alias HospitilizationsInitData =
    { id : Maybe Int
    , facilities : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
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


type alias HospitilizationsRow =
    { id : Int
    , facilityName : Maybe String
    , dateOfAdmission : Maybe String
    , admitProblem : Maybe String
    , dateOfDischarge : Maybe String
    , dischargeProblem : Maybe String
    , serviceType : Maybe String
    , fromTcm : Bool
    , recordId : Maybe Int
    , dropDownOpen : Bool

    -- for edit
    , patientId : Int
    , facilityId : Maybe Int
    , patientReported : Bool
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


type alias SomeDropDowns =
    { months : List DropDownItem
    , years : List DropDownItem
    }


monthDropdown : List DropDownItem
monthDropdown =
    [ DropDownItem Nothing ""
    , DropDownItem (Just 0) "January"
    , DropDownItem (Just 1) "February"
    , DropDownItem (Just 2) "March"
    , DropDownItem (Just 10) "October"
    , DropDownItem (Just 11) "November"
    , DropDownItem (Just 12) "December"
    ]


yearDropdown : List DropDownItem
yearDropdown =
    [ DropDownItem Nothing ""
    , DropDownItem (Just 2011) "2011"
    , DropDownItem (Just 2012) "2012"
    , DropDownItem (Just 2016) "2016"
    , DropDownItem (Just 2017) "2017"
    , DropDownItem (Just 2018) "2018"
    ]
