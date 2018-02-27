module Common.Types exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


type alias FilterState =
    { name : String
    , value : String
    }


type RequiredType
    = Required
    | Optional


type alias DropdownItem =
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


type alias AddEditDataSource =
    { facilityId : Maybe Int
    , patientId : Int
    , facilities : List DropdownItem
    , providers : List DropdownItem
    , recordTypes : List DropdownItem
    , users : List DropdownItem
    , tasks : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , hospitilizations : List DropdownItem
    }


monthDropdown : List DropdownItem
monthDropdown =
    [ DropdownItem Nothing ""
    , DropdownItem (Just 0) "January"
    , DropdownItem (Just 1) "February"
    , DropdownItem (Just 2) "March"
    , DropdownItem (Just 3) "April"
    , DropdownItem (Just 4) "May"
    , DropdownItem (Just 5) "June"
    , DropdownItem (Just 6) "July"
    , DropdownItem (Just 7) "August"
    , DropdownItem (Just 8) "September"
    , DropdownItem (Just 9) "October"
    , DropdownItem (Just 10) "November"
    , DropdownItem (Just 11) "December"
    ]


yearDropdown : List DropdownItem
yearDropdown =
    [ DropdownItem Nothing ""
    , DropdownItem (Just 2011) "2011"
    , DropdownItem (Just 2012) "2012"
    , DropdownItem (Just 2013) "2013"
    , DropdownItem (Just 2014) "2014"
    , DropdownItem (Just 2015) "2015"
    , DropdownItem (Just 2016) "2016"
    , DropdownItem (Just 2017) "2017"
    , DropdownItem (Just 2018) "2018"
    , DropdownItem (Just 2019) "2019"
    , DropdownItem (Just 2020) "2020"
    , DropdownItem (Just 2021) "2021"
    , DropdownItem (Just 2022) "2022"
    , DropdownItem (Just 2023) "2023"
    , DropdownItem (Just 2024) "2024"
    , DropdownItem (Just 2025) "2025"
    ]



-- PersonHeaderDetails Section


type alias PersonHeaderDetails =
    { patientId : Int
    , fullName : Maybe String
    , dateOfBirth : Maybe String
    , age : Maybe Int
    , nickname : Maybe String
    , facilityName : Maybe String
    , isVIP : Bool
    , hasNDA : Bool
    , contactHours : List String
    , mRN : Maybe String
    , pAN : Maybe String
    , pFID : Maybe String
    , primaryResource : Maybe String
    , restrictionsCount : Int
    , emailAddress : Maybe String
    , preferredLanguage : Maybe String
    , facilityId : Int
    , dateOfDeath : Maybe String
    , mainProvider : Maybe String
    }


emptyPersonHeaderDetails : PersonHeaderDetails
emptyPersonHeaderDetails =
    { patientId = 0
    , fullName = Nothing
    , dateOfBirth = Nothing
    , age = Nothing
    , nickname = Nothing
    , facilityName = Nothing
    , isVIP = False
    , hasNDA = False
    , contactHours = []
    , mRN = Nothing
    , pAN = Nothing
    , pFID = Nothing
    , primaryResource = Nothing
    , restrictionsCount = 0
    , emailAddress = Nothing
    , preferredLanguage = Nothing
    , facilityId = 0
    , dateOfDeath = Nothing
    , mainProvider = Nothing
    }
