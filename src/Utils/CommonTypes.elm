module Utils.CommonTypes exposing (..)


type RequiredType
    = Required
    | Optional


type alias Flags =
    { pageFlag : String
    , patientId : Int
    , recordType : Maybe Int
    }


type alias DropDownItem =
    { id : Maybe Int
    , name : String
    }


type alias DropDownState =
    { x : Float
    , y : Float
    , rowId : Int
    }


type RecordType
    = PrimaryCare
    | Specialty
    | Labs
    | Radiology
    | Hospitalizations
    | Legal
      -- | CCDS
    | CallRecordings
    | PreviousHistories
    | Enrollment
    | Misc



-- Feel like this should get used to CommonFunctions, but... later
-- Ccds -> ?


getRecordType : Int -> RecordType
getRecordType id =
    case id of
        1 ->
            PrimaryCare

        2 ->
            Specialty

        3 ->
            Labs

        4 ->
            Radiology

        5 ->
            Misc

        6 ->
            Legal

        9 ->
            Hospitalizations

        8 ->
            CallRecordings

        11 ->
            PreviousHistories

        12 ->
            Enrollment

        _ ->
            Debug.crash "Invalid recordId, cannot load program"


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
            8

        PreviousHistories ->
            11

        Enrollment ->
            12

        Misc ->
            5
