module Hospitilizations.Types exposing (Model, Filters, emptyModel)

import Common.Table as Table
import Common.Types exposing (HospitilizationsRow)


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
