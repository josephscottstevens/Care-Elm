module Utils.CommonTypes exposing (..)


type alias Flags =
    { pageFlag : String
    , patientId : Int
    , recordType : Maybe Int
    , facilityId : Maybe Int
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
