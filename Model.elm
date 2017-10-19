module Model exposing (..)

import Http
import Array


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


type Msg
    = Load (Result Http.Error Model)
    | EditStart Int
    | EditEnd
    | UpdateStartDate Int String
    | UpdateCity Int String
    | UpdateState Int String


type ModelState
    = Initial
    | Grid
    | Edit Int
    | Error Http.Error


type alias Model =
    { state : ModelState
    , patientId : Int
    , employers : Array.Array Employer
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , patientId = 0
    , employers = Array.empty
    }
