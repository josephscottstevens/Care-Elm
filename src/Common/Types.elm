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
    { facilityId : Maybe Int
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
    , hospitilizations : List DropDownItem
    , recordTypeId : Maybe Int
    , setFocus : Bool
    , isExistingHospitilization : Bool

    -- tt
    , timeVisit : Maybe String
    , timeAcc : Maybe String
    , fileName : String
    , facilityId : Maybe Int
    , facilityText : String
    , reportDate : Maybe String
    , recordingDate : Maybe String
    , userId : Maybe Int
    , userText : String
    , taskId : Maybe Int
    , taskText : String
    , hospitalizationId : Maybe Int
    , hospitalizationText : String
    , facilityId2 : Maybe Int
    , facilityText2 : String
    , dateOfAdmission : Maybe String
    , dateOfDischarge : Maybe String
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
    , hospitalServiceTypeId : Maybe Int
    , hospitalServiceTypeText : String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargePhysicianId : Maybe Int
    , dischargePhysicianText : String
    }


getAddEditMsg : AddEditDataSource -> RecordType -> Bool -> Bool -> RecordAddNewInitData
getAddEditMsg addEditDataSource recordType setFocus isExistingHospitilization =
    { facilityId = addEditDataSource.facilityId
    , facilities = addEditDataSource.facilities
    , recordTypes = addEditDataSource.recordTypes
    , users = addEditDataSource.users
    , tasks = addEditDataSource.tasks
    , hospitilizationServiceTypes = addEditDataSource.hospitilizationServiceTypes
    , hospitalizationDischargePhysicians = addEditDataSource.hospitalizationDischargePhysicians
    , hospitilizations = addEditDataSource.hospitilizations
    , recordTypeId = Just (getId recordType)
    , setFocus = setFocus
    , isExistingHospitilization = isExistingHospitilization

    -- no data from server, just filler data
    , timeVisit = Nothing
    , timeAcc = Nothing
    , fileName = ""
    , facilityText = ""
    , reportDate = Nothing
    , recordingDate = Nothing
    , userId = Nothing
    , userText = ""
    , taskId = Nothing
    , taskText = ""
    , hospitalizationId = Nothing
    , hospitalizationText = ""
    , facilityId2 = Nothing
    , facilityText2 = ""
    , dateOfAdmission = Nothing
    , dateOfDischarge = Nothing
    , dateOfAdmission2 = Nothing
    , dateOfDischarge2 = Nothing
    , hospitalServiceTypeId = Nothing
    , hospitalServiceTypeText = ""
    , admitDiagnosisId = Nothing
    , dischargeDiagnosisId = Nothing
    , dischargePhysicianId = Nothing
    , dischargePhysicianText = ""
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


monthDropdown : List DropDownItem
monthDropdown =
    [ DropDownItem Nothing ""
    , DropDownItem (Just 0) "January"
    , DropDownItem (Just 1) "February"
    , DropDownItem (Just 2) "March"
    , DropDownItem (Just 3) "April"
    , DropDownItem (Just 4) "May"
    , DropDownItem (Just 5) "June"
    , DropDownItem (Just 6) "July"
    , DropDownItem (Just 7) "August"
    , DropDownItem (Just 8) "May"
    , DropDownItem (Just 9) "October"
    , DropDownItem (Just 10) "November"
    , DropDownItem (Just 11) "December"
    ]


yearDropdown : List DropDownItem
yearDropdown =
    [ DropDownItem Nothing ""
    , DropDownItem (Just 2011) "2011"
    , DropDownItem (Just 2012) "2012"
    , DropDownItem (Just 2013) "2013"
    , DropDownItem (Just 2014) "2014"
    , DropDownItem (Just 2015) "2015"
    , DropDownItem (Just 2016) "2016"
    , DropDownItem (Just 2017) "2017"
    , DropDownItem (Just 2018) "2018"
    , DropDownItem (Just 2019) "2019"
    , DropDownItem (Just 2020) "2020"
    , DropDownItem (Just 2021) "2021"
    , DropDownItem (Just 2022) "2022"
    , DropDownItem (Just 2023) "2023"
    , DropDownItem (Just 2024) "2024"
    , DropDownItem (Just 2025) "2025"
    ]
