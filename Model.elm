module Model exposing (..)

import Http


type alias Employer =
    { rowId : Int
    , employmentStatus : String
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
    | EditStart Employer
    | EditSave Employer
    | EditCancel
    | UpdateStartDate String
    | UpdateCity Employer String
    | UpdateState Employer String
    | Reset


type ModelState
    = Initial
    | Grid
    | Edit Employer
    | Error Http.Error


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias Model =
    { state : ModelState
    , patientId : Int
    , employers : List Employer
    , sortCol : Maybe Employer
    , sortMode : SortMode
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , patientId = 0
    , employers = []
    , sortCol = Nothing
    , sortMode = SortNone
    }
