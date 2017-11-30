module Hospitilizations.Types exposing (..)

import Table
import Http
import Common.Types exposing (..)


type Msg
    = Load (Result Http.Error WebResponse)
    | SetTableState Table.State
    | SetFilter FilterState
    | DropDownToggle Int
    | EditTask Int
    | AddNewStart
    | DeleteConfirmed Int
    | DeleteCompleted (Result Http.Error String)
    | SendMenuMessage Int String


type ModelState
    = Grid
    | Limbo
    | Error String


type SortMode
    = SortNone
    | SortAsc
    | SortDesc


type alias WebResponse =
    { hospitilizations : List HospitilizationsRow
    }


type alias Model =
    { state : ModelState
    , hospitilizations : List HospitilizationsRow
    , patientId : Int
    , facilityId : Maybe Int
    , tableState : Table.State
    , query : String
    , filterFields : Filters
    }


emptyModel : Int -> Model
emptyModel patientId =
    { state = Grid
    , hospitilizations = []
    , patientId = patientId
    , facilityId = Nothing
    , tableState = Table.initialSort "Date"
    , query = ""
    , filterFields = emptyFilters
    }


type alias HospitilizationsRow =
    { id : Int
    , facilityName : Maybe String
    , dateOfAdmission : Maybe String
    , admitProblem : Maybe String
    , dateOfDischarge : Maybe String
    , dischargeProblem : Maybe String
    , serviceType : Maybe String
    , fromTcm : Bool
    , recordId : Maybe Int
    , dropDownOpen : Bool
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
