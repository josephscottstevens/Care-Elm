module Model exposing (..)

import Http


type alias Employer =
    { employmentStatus : String
    , occupation : String
    , employer : String
    , startDate : String
    , endDate : String
    , contactPerson : String
    , status : String
    , addressLine1 : String
    , addressLine2 : String
    , city : String
    , state : String
    , zipCode : String
    , phone : String
    , email : String
    , comment : String
    }


type alias Employment =
    { patientId : Int
    , employers : List Employer
    , testDate : String
    }


type Msg
    = Load (Result Http.Error Employment)
    | EditStart Employment
    | EditEnd Employment
    | UpdateTestDate Employment String


type ModelState
    = Initial
    | Grid Employment
    | Edit Employment
    | Error Http.Error


type alias Model =
    { state : ModelState }
