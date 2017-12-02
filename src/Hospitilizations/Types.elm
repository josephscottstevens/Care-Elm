module Hospitilizations.Types exposing (..)

import Table
import Http
import Common.Types exposing (FilterState)


type Msg
    = Load (Result Http.Error WebResponse)
    | SetTableState Table.State
    | SetFilter FilterState
    | DropDownToggle Int
    | Edit (Maybe Int)
    | AddNewStart (Maybe Int)
    | DeleteHospitilizationConfirmed Int
    | DeleteCompleted (Result Http.Error String)
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

    -- for edit
    , patientId : Int
    , facilityId : Maybe Int
    , patientReported : Bool
    , hospitalizationId : Maybe Int
    , hospitalServiceTypeId : Maybe Int
    , chiefComplaint : String
    , admitDiagnosisId : Maybe Int
    , dischargeDiagnosisId : Maybe Int
    , dischargeRecommendations : String
    , dischargePhysicianId : Maybe Int
    , facilityId2 : Maybe Int
    , dateOfAdmission2 : Maybe String
    , dateOfDischarge2 : Maybe String
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
