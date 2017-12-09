module Hospitilizations.Types exposing (..)

import Table
import Http
import Common.Types exposing (FilterState, HospitilizationsRow)


type Msg
    = Load (Result Http.Error WebResponse)
    | SetTableState Table.State
    | SetFilter FilterState
    | DropDownToggle Int
    | DeleteHospitilizationConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | HospitilizationsAdd
    | HospitilizationsEdit Int
    | SendMenuMessage Int String


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias WebResponse =
    { hospitilizations : List HospitilizationsRow
    }


type alias Model =
    { hospitilizations : List HospitilizationsRow
    , patientId : Int
    , facilityId : Maybe Int
    , tableState : Table.State
    , query : String
    , filterFields : Filters
    }


emptyModel : Int -> Model
emptyModel patientId =
    { hospitilizations = []
    , patientId = patientId
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
