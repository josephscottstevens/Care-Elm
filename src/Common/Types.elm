module Common.Types exposing (..)


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
    | ContinuityOfCareDocument


type alias AddEditDataSource =
    { facilityId : Maybe Int
    , facilities : List DropdownItem
    , providers : List DropdownItem
    , recordTypes : List DropdownItem
    , users : List DropdownItem
    , tasks : List DropdownItem
    , hospitilizationServiceTypes : List DropdownItem
    , hospitalizationDischargePhysicians : List DropdownItem
    , hospitilizations : List DropdownItem
    }


relationshipsDropdown : List DropdownItem
relationshipsDropdown =
    [ DropdownItem Nothing ""
    , DropdownItem (Just 0) "Spouse"
    , DropdownItem (Just 1) "Natural Child"
    , DropdownItem (Just 2) "Step Child"
    , DropdownItem (Just 3) "Foster Child"
    , DropdownItem (Just 4) "Grand Child"
    , DropdownItem (Just 5) "Niece/Nephew"
    , DropdownItem (Just 6) "Life partner"
    , DropdownItem (Just 7) "Significant other"
    , DropdownItem (Just 8) "Parent"
    , DropdownItem (Just 9) "Grandparent"
    , DropdownItem (Just 10) "Brother"
    , DropdownItem (Just 11) "Sister"
    , DropdownItem (Just 12) "Friend"
    , DropdownItem (Just 13) "Other"
    , DropdownItem (Just 14) "Ward of the court"
    ]


acuityLevelDropdown : List DropdownItem
acuityLevelDropdown =
    [ DropdownItem Nothing ""
    , DropdownItem (Just 0) "High"
    , DropdownItem (Just 1) "Medium"
    , DropdownItem (Just 2) "Low"
    ]


addressTypeDropdown : List DropdownItem
addressTypeDropdown =
    [ DropdownItem Nothing ""
    , DropdownItem (Just 0) "Home"
    , DropdownItem (Just 1) "Facility"
    ]


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


timeZoneDropdown : List DropdownItem
timeZoneDropdown =
    [ DropdownItem (Just 0) "Central Standard Time"
    ]
