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


type Msg
    = Load (Result Http.Error Model)
    | EditStart
    | EditEnd
    | UpdateTestDate String
    | UpdateFirst String
    | UpdateLast String


type ModelState
    = Initial
    | Grid
    | Edit
    | Error Http.Error


type alias Model =
    { state : ModelState
    , patientId : Int
    , employers : List Employer
    , testDate : String
    , testFirst : String
    , testLast : String
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , patientId = 0
    , testFirst = ""
    , testLast = ""
    , employers = []
    , testDate = ""
    }
