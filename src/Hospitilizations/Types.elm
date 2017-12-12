module Hospitilizations.Types exposing (..)

import Table
import Http
import Common.Types exposing (FilterState, HospitilizationsRow)


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias WebResponse =
    { hospitilizations : List HospitilizationsRow
    }


type alias Model =
    { hospitilizations : List HospitilizationsRow
    , facilityId : Maybe Int
    , tableState : Table.State
    , query : String
    , filterFields : Filters
    }


emptyModel : Model
emptyModel =
    { hospitilizations = []
    , facilityId = Nothing
    , tableState = Table.initialSort "Date"
    , query = ""
    , filterFields = emptyFilters
    }


type alias Filters =
    { id : String
    , facilityName : String
    , dateOfAdmission : String
    , admitProblem : String
    , dateOfDischarge : String
    , dischargeProblem : String
    , serviceType : String
    , fromTcm : String
    , recordId : String
    }


emptyFilters : Filters
emptyFilters =
    Filters "" "" "" "" "" "" "" "" ""


type alias FilterField =
    { fieldName : String
    , fieldText : String
    }
