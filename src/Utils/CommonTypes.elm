module Utils.CommonTypes exposing (..)


type alias Flags =
    { pageFlag : String
    , patientId : Int
    , recordType : Maybe Int
    }


type alias DropDownItem =
    { id : Maybe Int
    , name : String
    }
