module Model exposing (..)

import Http
import Table


type alias Employer =
    { rowId : Int
    , dob : String
    , email : String
    , addressLine1 : String
    , addressLine2 : String
    , city : String
    , state : String
    , zipCode : String
    , phone : String
    }


type Msg
    = Load (Result Http.Error Model)
    | EditStart Employer
    | EditSave Employer
    | EditCancel
    | UpdateStartDate String
    | UpdateCity Employer String
    | UpdateState Employer String
    | SetQuery String
    | SetTableState Table.State
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
    , tableState : Table.State
    , query : String
    }


emptyModel : Model
emptyModel =
    { state = Initial
    , patientId = 0
    , employers = []
    , tableState = Table.initialSort "dob"
    , query = ""
    }
