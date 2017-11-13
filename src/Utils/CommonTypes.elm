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
    | Speciality
    | Labs
    | Radiology
    | Misc
    | Legal
    | Hospitalizations
    | CallRecordings
    | PreviousHistories
    | Enrollment



-- Feel like this should get used to CommonFunctions, but... later


getRecordType : Int -> RecordType
getRecordType id =
    case id of
        1 ->
            PrimaryCare

        2 ->
            Speciality

        3 ->
            Labs

        4 ->
            Radiology

        5 ->
            Misc

        6 ->
            Legal

        7 ->
            Hospitalizations

        8 ->
            CallRecordings

        9 ->
            PreviousHistories

        10 ->
            Enrollment

        _ ->
            Debug.crash "Invalid recordId, cannot load program"


getId : RecordType -> Int
getId recordType =
    case recordType of
        PrimaryCare ->
            1

        Speciality ->
            2

        Labs ->
            3

        Radiology ->
            4

        Misc ->
            5

        Legal ->
            6

        Hospitalizations ->
            7

        CallRecordings ->
            8

        PreviousHistories ->
            9

        Enrollment ->
            10
