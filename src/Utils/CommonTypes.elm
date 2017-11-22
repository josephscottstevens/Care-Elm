module Utils.CommonTypes exposing (..)


type FilterState
    = FilterState String String


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


getRecordType : Maybe Int -> RecordType
getRecordType maybeId =
    case maybeId of
        Just id ->
            case id of
                1 ->
                    PrimaryCare

                2 ->
                    Specialty

                3 ->
                    Labs

                4 ->
                    Radiology

                9 ->
                    Hospitalizations

                6 ->
                    Legal

                10 ->
                    CallRecordings

                11 ->
                    PreviousHistories

                12 ->
                    Enrollment

                5 ->
                    Misc

                _ ->
                    Debug.crash "Invalid recordId, cannot load program"

        Nothing ->
            Debug.crash "Invalid recordId, cannot load program"


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
    , facilities : List DropDownItem
    , recordTypes : List DropDownItem
    , users : List DropDownItem
    , tasks : List DropDownItem
    , hospitilizationServiceTypes : List DropDownItem
    , hospitalizationDischargePhysicians : List DropDownItem
    }


type Page
    = Billing
    | Records
    | RecordAddNew (Maybe Int)


pageToString : Page -> ( String, Maybe Int )
pageToString page =
    case page of
        Billing ->
            ( "Billing", Nothing )

        Records ->
            ( "Records", Nothing )

        RecordAddNew t ->
            ( "RecordAddNew", t )
